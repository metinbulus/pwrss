#' Statistical Power for the Generic F-Test
#'
#' @description
#' Determines the power or the non-centrality parameter for the generic F-Test
#' with (optional) Type 1 and Type 2 error plots.
#'
#' @aliases power.f
#'
#'
#' @param power    statistical power \eqn{(1 - \beta)}; either `power` or `ncp`
#'                 needs to be NULL (and is then estimated).
#' @param ncp      non-centrality parameter for the alternative; either `power`
#'                  or `ncp` needs to be NULL (and is then estimated).
#' @param null.ncp non-centrality parameter for the null.
#' @param alpha    type 1 error rate, defined as the probability of incorrectly
#'                 rejecting a true null hypothesis, denoted as \eqn{\alpha}.
#' @param df1      integer; numerator degrees of freedom.
#' @param df2      integer; denominator degrees of freedom.
#' @param plot     logical; \code{FALSE} switches off Type 1 and Type 2 error
#'                 plot. \code{TRUE} by default.
#' @param verbose  \code{1} by default (returns test, hypotheses, and results),
#'                 if \code{2} a more detailed output is given (plus key
#'                 parameters and definitions), if \code{0} no output is printed
#'                 on the console.
#' @param utf      logical; whether the output should show Unicode characters
#'                 (if encoding allows for it). \code{FALSE} by default.
#'
#' @return
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{null.ncp}{non-centrality parameter under null.}
#'   \item{df1}{numerator degrees of freedom.}
#'   \item{df2}{denominator degrees of freedom.}
#'   \item{f.alpha}{critical value(s).}
#'
#' @examples
#' # power is defined as the probability of observing a test statistics greater
#' # than the critical value
#' power.f.test(ncp = 1, df1 = 4, df2 = 100, alpha = 0.05)
#' power.f.test(power = 0.80, df1 = 4, df2 = 100, alpha = 0.05)
#'
#' @export power.f.test
power.f.test <- function(power = NULL, ncp = NULL, null.ncp = 0, df1, df2,
                         alpha = 0.05, alternative = c("greater", "less"),
                         plot = TRUE, verbose = 1, utf = FALSE) {

  if (!is.null(power)) check.power(power)
  if (!is.null(ncp)) check.positive(ncp)
  check.nonnegative(null.ncp)
  check.positive(df1, df2)
  check.proportion(alpha)
  check.logical(plot, utf)
  alternative <- match.arg(alternative)
  verbose <- ensure.verbose(verbose)
  requested <- get.requested(es = ncp, n = NA, power = power)

  if (!is.null(ncp) && ncp < null.ncp)
    stop("`ncp` should be greater than or equal to `null.ncp`.", call. = FALSE)
  if (is.null(df1) || df1 < 1)
    stop("`df1` can not be NULL, and needs to be at least 1.", call. = FALSE)
  if (is.null(df2) || df2 < 3)
    stop("`df2` can not be NULL, and needs to be at least 3.", call. = FALSE)

  pwr <- function(ncp, null.ncp = 0, df1, df2, alpha = 0.05, alternative) {

    lower.tail <- alternative == "less"

    f.alpha <- stats::qf(alpha, df1 = df1, df2 = df2, ncp = null.ncp, lower.tail = lower.tail)
    power   <- stats::pf(f.alpha, df1 = df1, df2 = df2, ncp = ncp, lower.tail = lower.tail)

    list(power = power, f.alpha = f.alpha, alternative = alternative)

  } # pwr()

  min.pwr <- function(ncp, power, alternative) {
    power - pwr(ncp = ncp, null.ncp = null.ncp, df1 = df1, df2 = df2, alpha = alpha, alternative = alternative)$power
  } # min.pwr()

  if (requested == "es") {

     if (alternative == "less") {
       max.thresh <- null.ncp
     } else {
       max.thresh <- stats::qf(1 - 1e-12, ncp = null.ncp, df1 = df1, df2 = df2)
       while (min.pwr(max.thresh, power, alternative) > 0) max.thresh <- max.thresh * 1.10
    }

    # ncp <- stats::optimize(f = function(ncp) min.pwr(ncp, power, alternative) ^ 2, interval = c(0, max.thresh), tol = 1e-12)$minimum
    ncp <- stats::uniroot(
      f = function(ncp) min.pwr(ncp, power, alternative),
      interval = c(0, max.thresh),
      tol = 1e-12
    )$root

  }

  pwr.obj <- pwr(ncp = ncp, null.ncp = null.ncp, df1 = df1, df2 = df2, alpha = alpha, alternative = alternative)

  if (plot)
    suppressWarnings(.plot.f.t1t2(ncp = ncp, null.ncp = null.ncp, df1 = df1, df2 = df2, alpha = alpha, alternative = alternative))

  if (verbose > 0) {

    print.obj <- list(test = "Generic F-Test",
                      requested = requested,
                      tgt.ncp = "lambda",
                      lambda = ncp,
                      null.lambda = null.ncp,
                      df1 = df1,
                      df2 = df2,
                      n.pres = df1 + df2 + 1,
                      power = pwr.obj$power,
                      alpha = alpha,
                      alternative = alternative,
                      f.alpha = pwr.obj$f.alpha)

    .print.pwrss.f(print.obj, verbose = verbose, utf = utf)

  } # end of verbose

  invisible(structure(list(power = pwr.obj$power,
                           ncp = ncp,
                           null.ncp = null.ncp,
                           df1 = df1,
                           df2 = df2,
                           alpha = alpha,
                           f.alpha = pwr.obj$f.alpha,
                           alternative = alternative,
                           utf = utf),
                      class = c("pwrss", "generic", "f")))

} # end of power.f.test()

power.f <- power.f.test
