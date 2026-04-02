#' Statistical Power for the Generic Chi-Square Test
#'
#' @description
#' Calculates power for the generic chi-square test with (optional) Type 1 and
#' Type 2 error plots.
#'
#' @aliases power.chisq
#'
#'
#' @param ncp      non-centrality parameter for the alternative.
#' @param null.ncp non-centrality parameter for the null.
#' @param df       integer; degrees of freedom. For example, for the test of
#'                 independence df = (nrow - 1)*(ncol - 1).
#' @param alpha    type 1 error rate, defined as the probability of incorrectly
#'                 rejecting a true null hypothesis, denoted as \eqn{\alpha}.
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
#'   \item{df}{degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{ncp.null}{non-centrality parameter under null.}
#'   \item{alpha}{type 1 error rate (user-specified).}
#'   \item{chisq.alpha}{critical value.}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # power is defined as the probability of observing a test statistics greater
#' # than the critical value
#' power.chisq.test(ncp = 20, df = 100, alpha = 0.05)
#'
#' @export power.chisq.test
power.chisq.test <- function(ncp, null.ncp = 0, df, alpha = 0.05,
                             plot = TRUE, verbose = 1, utf = FALSE) {

  check.positive(ncp, df)
  check.nonnegative(null.ncp)
  check.proportion(alpha)
  check.logical(plot, utf)
  verbose <- ensure_verbose(verbose)

  if (ncp < null.ncp)
    stop("`ncp` should be greater than or equal to `null.ncp`.", call. = FALSE)

  chisq.alpha <- stats::qchisq(alpha, df = df, ncp = null.ncp, lower.tail = FALSE)

  power <- stats::pchisq(chisq.alpha, df = df, ncp = ncp, lower.tail = FALSE)

  if (plot)
    .plot.chisq.t1t2(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha)

  if (verbose > 0) {

    print.obj <- list(test = "Generic Chi-square Test",
                      requested = "power",
                      power = power, ncp.alternative = ncp, ncp.null = null.ncp,
                      alpha = alpha, chisq.alpha = chisq.alpha, df = df)

    .print.pwrss.chisq(print.obj, verbose = verbose, utf = utf)

  } # end of verbose

  invisible(list(power = power, ncp = ncp, null.ncp = null.ncp, alpha = alpha, df = df, chisq.alpha = chisq.alpha))

} # end of power.chisq.test()

power.chisq <- power.chisq.test


#' Find Non-Centrality Parameter for the Generic Chi-Square Test
#'
#' @description
#' Finds non-centrality parameter and degress of freedom for the generic chi-square test with (optional) Type 1 and
#' Type 2 error plots.
#'
#' @aliases ncp.chisq
#'
#' @param power    statistical power \eqn{(1-\beta)}.
#' @param ncp      non-centrality parameter for the alternative.
#' @param null.ncp non-centrality parameter for the null.
#' @param df       integer; degrees of freedom. For example, for the test of
#'                 independence df = (nrow - 1)*(ncol - 1).
#' @param alpha    type 1 error rate, defined as the probability of incorrectly
#'                 rejecting a true null hypothesis, denoted as \eqn{\alpha}.
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
#'   \item{df}{degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{ncp.null}{non-centrality parameter under null.}
#'   \item{alpha}{type 1 error rate (user-specified).}
#'   \item{chisq.alpha}{critical value.}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # power is defined as the probability of observing a test statistics greater
#' # than the critical value
#' power.chisq.test(ncp = 20, df = 100, alpha = 0.05)
#'
#' @export ncp.chisq.test
ncp.chisq.test <- function(power = 0.80, ncp = NULL, null.ncp = 0,
                           df = NULL, alpha = 0.05,
                           plot = TRUE, verbose = 1, utf = FALSE) {
  
  if(is.null(ncp)) {
    
    if(is.null(df)) stop("'df' cannot be NULL", call. = FALSE)
    if(df < 1) stop("Degrees of freedom cannot be smaller than 1.", call. = FALSE)
    
    max.null <- qchisq(1 - 1e-10, ncp = null.ncp, df = df)
    max <- qchisq(1 - 1e-10, ncp = max.null, df = df)
    
    ncp <- optimize(
      f = function(ncp) {
        (power - power.chisq.test(ncp = ncp, null.ncp = null.ncp,
                                  df = df, alpha = alpha, 
                                  plot = FALSE, verbose = 0, utf = FALSE)$power)^2
      },
      maximum = FALSE,
      lower = 0,
      upper = max,
    )$minimum
    
  } # ncp is null
  
  if(is.null(df)) {
    
    if(is.null(ncp)) stop("'ncp' cannot be NULL", call. = FALSE)
    
    df <- optimize(
      f = function(df) {
        (power - power.chisq.test(ncp = ncp, null.ncp = null.ncp,
                                  df = df, alpha = alpha,
                                  plot = FALSE, verbose = 0, utf = FALSE)$power)^2
      },
      maximum = FALSE,
      lower = 1,
      upper = 1e10,
    )$minimum
    
  } # df is null
  
  pwrss::power.chisq.test(ncp = ncp, null.ncp = null.ncp,
                          df = df, alpha = alpha,
                          plot = plot, verbose = verbose, utf = utf)
  
} # ncp.chisq.test

ncp.chisq <- ncp.chisq.test
