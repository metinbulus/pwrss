#' Statistical Power for the Lambda-Prime Distribution
#'
#' @description
#' Calculates power for the lambda-prime distribution with (optional) Type 1 and Type 2
#' error plots.
#'
#' @aliases power.lp
#'
#'
#' @param             ncp non-centrality parameter for the alternative.
#' @param null.ncp    non-centrality parameter for the null. When alternative =
#'                    "two.one.sided", the function expects two values in the
#'                    form `c(lower, upper)`. If a single value is provided, it
#'                    is interpreted as the absolute bound and automatically
#'                    expanded to `c(-value, +value)`.
#' @param df          degrees of freedom.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided", "one.sided", or "two.one.sided".
#'                    "two.one.sided" is used for equivalence and minimal
#'                    effect testing.
#' @param plot        logical; \code{FALSE} switches off Type 1 and Type 2
#'                    error plot. \code{TRUE} by default.
#' @param verbose     \code{1} by default (returns test, hypotheses, and
#'                    results), if \code{2} a more detailed output is given
#'                    (plus key parameters and definitions), if \code{0} no
#'                    output is printed on the console.
#' @param utf         logical; whether the output should show Unicode
#'                    characters (if encoding allows for it). \code{FALSE} by
#'                    default.
#'
#' @return
#'   \item{df}{degrees of freedom.}
#'   \item{ncp}{non-centrality parameter under alternative.}
#'   \item{ncp.null}{non-centrality parameter under null.}
#'   \item{alpha}{type 1 error rate (user-specified).}
#'   \item{t.alpha}{critical value(s).}
#'   \item{beta}{type 2 error rate.}
#'   \item{type.s}{type S error rate (only for two-tailed test).}
#'   \item{type.m}{type M error rate (only for two-tailed test).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # two-sided
#' # power defined as the probability of observing test statistics greater
#' # than the positive critical value OR less than the negative critical value
#' power.lp.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided")
#'
#' # one-sided
#' # power is defined as the probability of observing a test statistic greater
#' # than the critical value
#' power.lp.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "one.sided")
#'
#' # equivalence
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the lower bound) AND less than the
#' # lower critical value (for the upper bound)
#' power.lp.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' # minimal effect testing
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the upper bound) OR less than the lower
#' # critical value (for the lower bound).
#' power.lp.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' @export power.lp.test
power.lp.test <- function(ncp, null.ncp = 0,
                         df, alpha = 0.05,
                         alternative = c("two.sided", "one.sided", "two.one.sided"),
                         plot = TRUE, verbose = 1, utf = FALSE) {
  
  alternative <- tolower(match.arg(alternative))
  
  check.numeric(ncp)
  null.ncp <- check.margins(null.ncp, check.numeric, alternative)
  if (!is.numeric(df) || length(df) != 1 || df < 1)
    stop("`df` must be numeric, have a value of at least 1 and have a length of 1.", call. = FALSE)
  check.proportion(alpha)
  check.logical(plot, utf)
  verbose <- ensure_verbose(verbose)
  
  if(ncp > 40 || any(null.ncp > 40)) stop("Consider using a z-test. Lambda-prime distribution with large non-centrality can be unreliable", call. = FALSE)
  
  # calculate statistical power
  if (alternative == "two.sided") {
    
      t.alpha <- sadists::qlambdap(p = alpha / 2, df = df, t = 0, lower.tail = FALSE)
      power <- 1 - sadists::plambdap(q = t.alpha, df = df, t = abs(ncp)) + sadists::plambdap(q = -t.alpha, df = df, t = abs(ncp))

    # t.alpha.s <- qt(p = 1 - alpha / 2, df = df, ncp = null.ncp)
    # type.s <- pt(q = -t.alpha.s, df = df, ncp = ncp) / 
    #  (pt(q = -t.alpha.s, df = df, ncp = ncp) +
    #     (1 - pt(q = t.alpha.s, df = df, ncp = ncp)))
    
    Phi.p <- pt(q = max(t.alpha), df = df, ncp = ncp)  
    Phi.m <- pt(q = min(t.alpha), df = df, ncp = ncp)  
    type.s <- min(Phi.m, 1 - Phi.p) / (Phi.m + 1 - Phi.p)
    
    type.m <- suppressWarnings({ 
      bounds <- sadists::qlambdap(c(1e-10, 1 - 1e-10), df = df, t = ncp)  
      integrand <- function(t) abs(t) * sadists::dlambdap(t, df = df, t = ncp)
      numerator <- integrate(integrand, min(bounds), min(t.alpha))$value +
        integrate(integrand, max(t.alpha), max(bounds))$value
      denominator  <- abs(ncp) * (sadists::plambdap(min(t.alpha), df = df, t = ncp) + sadists::plambdap(max(t.alpha), df = df, t = ncp, lower.tail = FALSE))
      numerator / denominator 
    })
    
  } else if (alternative == "one.sided") {
    
    lower.tail <- ncp < null.ncp
    t.alpha  <- sadists::qlambdap(p = alpha, df = df, t = null.ncp,  lower.tail = lower.tail)
    power <- sadists::plambdap(q = t.alpha, df = df, t = ncp,  lower.tail = lower.tail) 
    
    type.s <- 0
    type.m <- NA
    
  } else if (alternative == "two.one.sided" && (ncp > min(null.ncp) && ncp < max(null.ncp))) {  # equivalence test
    
    t.alpha.left <- sadists::qlambdap(p = 1 - alpha, df = df, t = max(null.ncp), lower.tail = FALSE)
    t.alpha.right <- sadists::qlambdap(p = alpha, df = df, t = min(null.ncp), lower.tail = FALSE)
    power <- sadists::plambdap(q = t.alpha.left, df = df, t = ncp) - 
      sadists::plambdap(q = t.alpha.right, df = df, t = ncp)
    
    power[power < 0] <- 0
    
    t.alpha <- c(t.alpha.left,  t.alpha.right)
    
    type.s <- NA
    type.m <- NA
    
  } else if (alternative == "two.one.sided" && (ncp < min(null.ncp) || ncp > max(null.ncp))) {  # minimum effect test
    
    t.alpha.right <- sadists::qlambdap(p = alpha / 2, df = df, t = max(null.ncp), lower.tail = FALSE)
    t.alpha.left <- sadists::qlambdap(p = alpha / 2, df = df, t = min(null.ncp), lower.tail = TRUE)
    power <- sadists::plambdap(q = t.alpha.right, df = df, t = ncp, lower.tail = FALSE) +
      sadists::plambdap(q = t.alpha.left, df = df, t = ncp, lower.tail = TRUE)
    
    t.alpha <- c(t.alpha.left,  t.alpha.right)
    
    type.s <- NA
    type.m <- NA
    
  }
  
  if (plot)
    suppressWarnings(.plot.lp.t1t2(ncp = ncp, null.ncp = null.ncp, df = df, alpha = alpha, alternative = alternative))
  
  if (verbose > 0) {
    
    print.obj <- list(test = "Generic Lambda-Prime Distribution",
                      requested = "power",
                      alternative = alternative,
                      ncp.alternative = ncp,
                      ncp.null = null.ncp,
                      t.alpha = t.alpha,
                      df = df, alpha = alpha,
                      power = power)
    
    .print.pwrss.t(print.obj, verbose = verbose, utf = utf)
    
  } # verbose
  
  invisible(list(alternative = alternative, ncp = ncp, null.ncp = null.ncp,
                 df = df, alpha = alpha, t.alpha = t.alpha, beta = 1 - power, 
                 type.s = type.s, type.m = type.m, power = power))
  
} # end of power.lp.test()

power.lp <- power.lp.test
