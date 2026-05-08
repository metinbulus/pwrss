#' Statistical Power for the Generic F-Test
#'
#' @description
#' Calculates power for the generic F-Test with (optional) Type 1 and Type 2
#' error plots.
#'
#' @aliases power.f
#'
#'
#' @param          ncp non-centrality parameter for the alternative.
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
#' @param utf logical; whether the output should show Unicode characters (if
#' encoding allows for it). \code{FALSE} by default.
#'
#' @return
#'   \item{df1}{numerator degrees of freedom.}
#'   \item{df2}{denominator degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{ncp.null}{non-centrality parameter under null.}
#'   \item{f.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # power is defined as the probability of observing a test statistics greater
#' # than the critical value
#' power.f.test(ncp = 1, df1 = 4, df2 = 100, alpha = 0.05)
#'
#' @export power.f.test
power.f.test <- function(ncp, null.ncp = 0, df1, df2, alpha = 0.05,
                         plot = TRUE, verbose = 1, utf = FALSE) {

  if (!is.null(ncp)) check.positive(ncp)
  check.nonnegative(null.ncp)
  check.positive(df1, df2)
  check.proportion(alpha)
  check.logical(plot, utf)
  verbose <- ensure.verbose(verbose)

  if (ncp < null.ncp)
    stop("`ncp` should be greater than or equal to `null.ncp`.", call. = FALSE)

  f.alpha <- stats::qf(alpha, df1 = df1, df2 = df2, ncp = null.ncp, lower.tail = FALSE)
  power   <- stats::pf(f.alpha, df1 = df1, df2 = df2, ncp = ncp, lower.tail = FALSE)

  if (plot)
    suppressWarnings(.plot.f.t1t2(ncp = ncp, null.ncp = null.ncp, df1 = df1, df2 = df2, alpha = alpha))

  if (verbose > 0) {

    print.obj <- list(test = "Generic F-Test",
                      requested = "power",
                      power = power, ncp.alternative = ncp, ncp.null = null.ncp,
                      alpha = alpha, f.alpha = f.alpha, df1 = df1, df2 = df2)

    .print.pwrss.f(print.obj, verbose = verbose, utf = utf)

  } # end of verbose

  invisible(list(power = power, ncp = ncp, null.ncp = null.ncp, alpha = alpha, df1 = df1, df2 = df2, f.alpha = f.alpha))

} # end of power.f.test()

power.f <- power.f.test

#' Find Non-Centrality Parameter for the Generic F-Test
#'
#' @description
#' Finds the non-centrality parameter for the generic F-Test with (optional) Type 1 and Type 2
#' error plots.
#'
#' @aliases ncp.f
#'
#' @param power    statistical power \eqn{(1-\beta)}.
#' @param ncp      non-centrality parameter for the alternative.
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
#' @param utf logical; whether the output should show Unicode characters (if
#' encoding allows for it). \code{FALSE} by default.
#'
#' @return
#'   \item{df1}{numerator degrees of freedom.}
#'   \item{df2}{denominator degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{ncp.null}{non-centrality parameter under null.}
#'   \item{f.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # power is defined as the probability of observing a test statistics greater
#' # than the critical value
#' ncp.f.test(ncp = NULL, power = 0.80, df1 = 4, df2 = 100, alpha = 0.05)
#'
#' @export ncp.f.test
ncp.f.test <- function(power = 0.80, ncp = NULL, null.ncp = 0, df1 = NULL, df2 = NULL,
                       alpha = 0.05, plot = TRUE, verbose = 1, utf = FALSE) {

  check.power(power)
  check.numeric(null.ncp)
  if (is.null(df1) || df1 < 1) stop("`df1` can not be NULL, and needs to be at least 1.", call. = FALSE)
  if (is.null(df2) || df2 < 3) stop("`df2` can not be NULL, and needs to be at least 3.", call. = FALSE)
  check.proportion(alpha)
  check.logical(plot, utf)
  verbose <- ensure.verbose(verbose)

  if (!is.null(ncp))
    stop("`ncp` needs to be NULL.", call. = FALSE)
  
  max.thresh <- stats::qf(1 - 1e-10, ncp = null.ncp, df1 = df1, df2 = df2)
  while (power.f.test(ncp = max.thresh, null.ncp = null.ncp, df1 = df1, df2 = df2, alpha = alpha,
                      plot = FALSE, verbose = 0, utf = FALSE)$power <= power) {
    max.thresh <- max.thresh * 1.10
  }

  ncp <- stats::optimize(
    f = function(ncp) {
      (power - power.f.test(ncp = ncp, null.ncp = null.ncp,
                            df1 = df1, df2 = df2, alpha = alpha,
                            plot = FALSE, verbose = 0, utf = FALSE)$power) ^ 2
    },
    maximum = FALSE, lower = 0, upper = max.thresh)$minimum

  power.f.test(ncp = ncp, null.ncp = null.ncp, df1 = df1, df2 = df2, alpha = alpha, plot = FALSE, verbose = 0)

} # ncp.f.test

ncp.f <- ncp.f.test
