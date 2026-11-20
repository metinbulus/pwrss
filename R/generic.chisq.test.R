#' Statistical Power for the Generic Chi-Square Test
#'
#' @description
#' Determines power, the non-centrality parameter (NCP) for the generic
#' chi-square test with (optional) Type 1 and Type 2 error plots.
#'
#' @aliases power.chisq
#'
#'
#' @param power        statistical power \eqn{(1 - \beta)}; either `power`, `ncp`
#'                     or `df` needs to be NULL (and is then estimated).
#' @param ncp          non-centrality parameter for the alternative; either
#'                     `power`, `ncp` or `df` needs to be NULL (and is then
#'                     estimated).
#' @param null.ncp     non-centrality parameter for the null.
#' @param df           integer; degrees of freedom, e.g., for the test of
#'                     independence df = (nrow - 1) * (ncol - 1); either `power`,
#'                     `ncp` or `df` needs to be NULL (and is then estimated).
#' @param alpha        type 1 error rate, defined as the probability of incorrectly
#'                     rejecting a true null hypothesis, denoted as \eqn{\alpha}.
#' @param alternative  character; the direction or type of the hypothesis test:
#'                     either "less" or "greater"
#' @param plot         logical; \code{FALSE} switches off Type 1 and Type 2 error
#'                     plot. \code{TRUE} by default.
#' @param verbose      \code{1} by default (returns test, hypotheses, and results),
#'                     if \code{2} a more detailed output is given (plus key
#'                     parameters and definitions), if \code{0} no output is
#'                     printed on the console.
#' @param utf          logical; whether the output should show Unicode characters
#'                     (if encoding allows for it). \code{FALSE} by default.
#'
#' @return
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{null.ncp}{non-centrality parameter under null.}
#'   \item{df}{degrees of freedom.}
#'   \item{alpha}{type 1 error rate (user-specified).}
#'   \item{chisq.alpha}{critical value.}
#'   \item{alternative}{non-centrality parameter under null.}
#'
#' @examples
#' # power is defined as the probability of observing a test statistics greater
#' # than the critical value
#' power.chisq.test(ncp = 20, df = 100, alpha = 0.05)
#' power.chisq.test(power = 0.80, df = 100, alpha = 0.05)
#' power.chisq.test(power = 0.80, ncp = 20, alpha = 0.05)
#'
#' @export power.chisq.test
power.chisq.test <- function(power = NULL, ncp = NULL, null.ncp = 0,
                             df = NULL, alpha = 0.05,
                             alternative = c("greater", "less"),
                             plot = TRUE, verbose = 1, utf = FALSE) {

  if (!is.null(power)) check.power(power)
  if (!is.null(ncp)) check.positive(ncp)
  check.nonnegative(null.ncp)
  if (!is.null(df)) check.positive(df)
  check.proportion(alpha)
  alternative <- match.arg(alternative)
  check.logical(plot, utf)
  verbose <- ensure.verbose(verbose)
  requested <- get.requested(es = ncp, n = df, power = power)

  if (!is.null(ncp) && alternative == "less" && ncp > null.ncp)
    stop("`ncp` should be less than or equal to `null.ncp`.", call. = FALSE)
  if (!is.null(ncp) && alternative == "greater" && ncp < null.ncp)
    stop("`ncp` should be greater than or equal to `null.ncp`.", call. = FALSE)
  if (!is.null(df) && df < 1)
    stop("Degrees of freedom can not be smaller than 1.", call. = FALSE)

  pwr <- function(ncp = NULL, null.ncp = 0, df = NULL, alpha = 0.05,
                  alternative) {

    chisq.alpha <- stats::qchisq(alpha, df = df, ncp = null.ncp, lower.tail = alternative == "less")
    power <- stats::pchisq(chisq.alpha, df = df, ncp = ncp, lower.tail = alternative == "less")

    list(power = power, chisq.alpha = chisq.alpha, alternative = alternative)

  } # pwr()

  min.pwr <- function(ncp = NULL, df = NULL, power = NULL, alternative) {

    power - pwr(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha, alternative = alternative)$power

  } # min.pwr() (for uniroot and optimize)

  if (requested == "es") {

    if (alternative == "less") {
      max.thresh <- null.ncp
    } else {
      max.thresh <- stats::qchisq(1 - 1e-10, ncp = null.ncp, df = df)
      while (min.pwr(max.thresh, df, power, alternative) > 0) max.thresh <- max.thresh * 1.10
    }

    # ncp <- stats::optimize(f = function(ncp) min.pwr(ncp, df, power, alternative) ^ 2, interval = c(0, max.thresh), tol = 1e-12)$minimum
    ncp <- stats::uniroot(
      f = function(ncp) min.pwr(ncp, df, power, alternative),
      interval = c(0, max.thresh),
      tol = 1e-12
    )$root

  } else if (requested == "n") {

    # df  <- stats::optimize(f = function(df)  min.pwr(ncp, df, power, alternative) ^ 2, interval = c(1, 1e10))$minimum
    df <- stats::uniroot(
      f = function(df) min.pwr(ncp, df, power, alternative),
      interval = c(1, 1e10),
      tol = 1e-12
    )$root

  }

  pwr.obj <- pwr(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha, alternative = alternative)

  if (plot)
    .plot.chisq.t1t2(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha, alternative = alternative)

  if (verbose > 0) {

    print.obj <- list(test = "Generic Chi-square Test",
                      requested = requested,
                      tgt.ncp = "lambda",
                      lambda = ncp,
                      null.lambda = null.ncp,
                      power = pwr.obj$power,
                      df = df,
                      alpha = alpha,
                      alternative = alternative,
                      chisq.alpha = pwr.obj$chisq.alpha)

    .print.pwrss.chisq(print.obj, verbose = verbose, utf = utf)

  } # end of verbose

  invisible(structure(list(power = pwr.obj$power,
                           ncp = ncp,
                           null.ncp = null.ncp,
                           df = df,
                           alpha = alpha,
                           chisq.alpha = pwr.obj$chisq.alpha,
                           alternative = alternative,
                           utf = utf),
                      class = c("pwrss", "generic", "chisq")))

} # end of power.chisq.test()


# statistic: observed chi-square test statistic
# probs: inversion probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.chisq.test <- function(statistic, df, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  check.numeric(statistic)
  check.positive(df)
  check.vector(probs, check.proportion)

  find.ncp.chisq <- function(target, no.root = 0) {

    objective <- function(ncp) suppressWarnings(stats::pchisq(statistic, df = df, ncp = ncp)) - target

    # Evaluate the objective at the smallest possible ncp.
    value.at.zero <- objective(0)

    if (!is.finite(value.at.zero)) return(NA_real_)

    # the CDF decreases as ncp increases, if the CDF is already below the target at ncp = 0, no nonnegative root exists
    if (value.at.zero < 0) return(no.root)

    # start searching with an upper boundary of 1.
    upper <- 1

    # increase the upper boundary until the objective changes from positive to negative
    for (i in seq_len(500L)) {

      value.at.upper <- objective(upper)
      if (is.finite(value.at.upper) && value.at.upper <= 0) break

      upper <- upper * 2
    }

    # if no valid upper boundary was found, return NA
    if (!exists("value.at.upper") || !is.finite(value.at.upper) || value.at.upper > 0) return(NA_real_)

    tryCatch(stats::uniroot(objective, interval = c(0, upper), tol = 1e-12, maxiter = 500L)$root, error = function(e) NA_real_)

  } # find.ncp.chisq

  ci <- vapply(1 - probs, function(target) find.ncp.chisq(target = target), numeric(1L))

  list(statistic = statistic, df = df, probs = probs, ci = setNames(ci, paste0(round(probs * 100, 1), "%")))

} # precision.chisq.test()


# aliases
power.chisq     <- power.chisq.test
precision.chisq <- precision.chisq.test
