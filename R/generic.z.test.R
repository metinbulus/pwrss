#' Statistical Power for the Generic z-Test
#'
#' @description
#' Calculates power for the generic z-Test with (optional) Type 1 and Type 2
#' error plots.
#'
#' @aliases power.z
#'
#'
#' @param             mean mean of the alternative.
#' @param sd          standard deviation of the alternative. Do not change this
#'                    value except when some sort of variance correction is
#'                    applied (e.g. as in logistic and Poisson regressions).
#' @param null.mean   mean of the null. When alternative = "two.one.sided", the
#'                    function expects two values in the form `c(lower,
#'                    upper)`. If a single value is provided, it is interpreted
#'                    as the absolute bound and automatically expanded to
#'                    `c(-value, +value)`.
#' @param null.sd     standard deviation of the null. Do not change this value
#'                    except when some sort of correction is applied.
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
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{alpha}{type 1 error rate (user-specified).}
#'   \item{z.alpha}{critical value(s).}
#'   \item{beta}{type 2 error rate.}
#'   \item{type.s}{type S error rate (only for two-tailed test).}
#'   \item{type.m}{type M error rate (only for two-tailed test).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # two-sided
#' # power defined as the probability of observing test statistics greater than
#' # the positive critical value OR less than the negative critical value
#' power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided")
#'
#' # one-sided
#' # power is defined as the probability of observing a test statistic greater
#' # than the critical value
#' power.z.test(mean = 1.96, alpha = 0.05, alternative = "one.sided")
#'
#' # equivalence
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the lower bound) AND less than the
#' # lower critical value (for the upper bound)
#' power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' # minimal effect testing
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the upper bound) OR less than the lower
#' # critical value (for the lower bound).
#' power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' @export power.z.test
power.z.test <- function(mean = NULL, sd = 1, null.mean = 0, null.sd = 1,
                         alpha = 0.05, alternative = c("two.sided", "one.sided", "two.one.sided"),
                         plot = TRUE, verbose = 1, utf = FALSE) {
  
  alternative <- tolower(match.arg(alternative))
  
  check.numeric(mean)
  check.positive(sd)
  null.mean <- check.margins(null.mean, check.numeric, alternative)
  check.nonnegative(null.sd)
  check.proportion(alpha)
  check.logical(plot, utf)
  verbose <- ensure_verbose(verbose)
  
  # calculate statistical power
  if (alternative == "two.sided") {
    
    z.alpha <- c(stats::qnorm(alpha / 2,  mean = null.mean,      sd = null.sd, lower.tail = TRUE),
                 stats::qnorm(alpha / 2,  mean = null.mean,      sd = null.sd, lower.tail = FALSE))
    power   <-   stats::pnorm(z.alpha[1], mean = mean,           sd = sd,      lower.tail = TRUE) +
      stats::pnorm(z.alpha[2], mean = mean,           sd = sd,      lower.tail = FALSE)
    
    # z.alpha.s <- qnorm(p = 1 - alpha / 2, mean = null.mean, sd = null.sd)
    # type.s <- pnorm(q = -z.alpha.s, mean = mean, sd = sd) /
    #  (pnorm(q = -z.alpha.s, mean = mean, sd = sd) + 
    #     (1 - pnorm(q = z.alpha.s, mean = mean, sd = sd)))
    
    Phi.p <- pnorm(q = max(z.alpha), mean = mean, sd = sd)  
    Phi.m <- pnorm(q = min(z.alpha), mean = mean, sd = sd)  
    type.s <- min(Phi.m, 1 - Phi.p) / (Phi.m + 1 - Phi.p)
    
    phi.p <- dnorm(x = max(z.alpha), mean = mean, sd = sd) 
    phi.m <- dnorm(x = min(z.alpha), mean = mean, sd = sd) 
    Phi.p <- pnorm(q = max(z.alpha), mean = mean, sd = sd)  
    Phi.m <- pnorm(q = min(z.alpha), mean = mean, sd = sd) 
    type.m <- (sd^2 * (phi.p + phi.m) + mean * (1 - Phi.p - Phi.m)) /
      (abs(mean) * (1 - Phi.p + Phi.m))
    
  } else if (alternative == "one.sided") {
    
    lower.tail <- mean < null.mean
    z.alpha <- stats::qnorm(alpha,        mean = null.mean,      sd = null.sd, lower.tail = lower.tail)
    power   <- stats::pnorm(z.alpha,      mean = mean,           sd = sd,      lower.tail = lower.tail)
    
    type.s <- 0
    type.m <- NA
    
  } else if (alternative == "two.one.sided" && (mean > min(null.mean) && mean < max(null.mean))) {  # equivalence test
    
    z.alpha <- c(stats::qnorm(alpha,      mean = min(null.mean), sd = null.sd, lower.tail = FALSE),
                 stats::qnorm(alpha,      mean = max(null.mean), sd = null.sd, lower.tail = TRUE))
    power   <-   stats::pnorm(z.alpha[2], mean = mean,           sd = sd,      lower.tail = TRUE) +
      stats::pnorm(z.alpha[1], mean = mean,           sd = sd,      lower.tail = FALSE) - 1
    power[power < 0] <- 0
    
    type.s <- NA
    type.m <- NA
    
  } else if (alternative == "two.one.sided" && (mean < min(null.mean) || mean > max(null.mean))) {  # minimum effect test
    
    z.alpha <- c(stats::qnorm(alpha / 2,  mean = min(null.mean), sd = null.sd, lower.tail = TRUE),
                 stats::qnorm(alpha / 2,  mean = max(null.mean), sd = null.sd, lower.tail = FALSE))
    power   <-   stats::pnorm(z.alpha[1], mean = mean,           sd = sd,      lower.tail = TRUE) +
      stats::pnorm(z.alpha[2], mean = mean,           sd = sd,      lower.tail = FALSE)
    
    type.s <- NA
    type.m <- NA
    
  }
  
  if (plot) {
    # if (sd != 1 || null.sd != 1)
    #   stop("Plotting is not available when the standard deviation of the standard normal distribution deviates from one.", call. = FALSE)
    
    try(silent = TRUE,
        suppressWarnings(.plot.t.t1t2(ncp = mean, null.ncp = null.mean, df = Inf, alpha = alpha, alternative = alternative))
    ) # try
    
  }
  
  if (verbose > 0) {
    
    print.obj <- list(test = "Generic z-Test",
                      requested = "power",
                      alternative = alternative,
                      mean.alternative = mean,
                      sd.alternative = sd,
                      mean.null = null.mean,
                      sd.null = null.sd,
                      alpha = alpha,
                      z.alpha = z.alpha,
                      power = power)
    
    .print.pwrss.z(print.obj, verbose = verbose, utf = utf)
    
  } # verbose
  
  invisible(list(alternative = alternative, mean = mean, sd = sd, null.mean = null.mean, null.sd = null.sd,
                 alpha = alpha, z.alpha = z.alpha, beta = 1 - power, type.s = type.s, type.m = type.m, power = power))
  
} # end of power.z.test()

power.z <- power.z.test



#' Finds the Mean (Non-centrality Parameter) for the Generic z-Test
#'
#' @description
#' Finds the mean (non-centrality parameter) for the generic z-Test with (optional) Type 1 and Type 2
#' error plots.
#'
#' @aliases mean.z
#'
#' @param power       statistical power \eqn{(1-\beta)}.
#' @param mean        mean of the alternative.
#' @param sd          standard deviation of the alternative. Do not change this
#'                    value except when some sort of variance correction is
#'                    applied (e.g. as in logistic and Poisson regressions).
#' @param null.mean   mean of the null. When alternative = "two.one.sided", the
#'                    function expects two values in the form `c(lower,
#'                    upper)`. If a single value is provided, it is interpreted
#'                    as the absolute bound and automatically expanded to
#'                    `c(-value, +value)`.
#' @param null.sd     standard deviation of the null. Do not change this value
#'                    except when some sort of correction is applied.
#' @param sign        whether 'mean' is expected to be greater '+1', less than '-1', or within '0' the null.mean' bounds.
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
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{alpha}{type 1 error rate (user-specified).}
#'   \item{z.alpha}{critical value(s).}
#'   \item{beta}{type 2 error rate.}
#'   \item{type.s}{type S error rate (only for two-tailed test).}
#'   \item{type.m}{type M error rate (only for two-tailed test).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # two-sided
#' # power defined as the probability of observing test statistics greater than
#' # the positive critical value OR less than the negative critical value
#' mean.z.test(mean = NULL, alpha = 0.05, alternative = "two.sided")
#'
#' # one-sided
#' # power is defined as the probability of observing a test statistic greater
#' # than the critical value
#' mean.z.test(mean = NULL, alpha = 0.05, alternative = "one.sided")
#'
#' # equivalence
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the lower bound) AND less than the
#' # lower critical value (for the upper bound)
#' mean.z.test(mean = NULL, null.mean = c(-2, 2), alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' # minimal effect testing
#' # power is defined as the probability of observing a test statistic greater
#' # than the upper critical value (for the upper bound) OR less than the lower
#' # critical value (for the lower bound).
#' mean.z.test(mean = NULL, null.mean = c(-1, 1), alpha = 0.05,
#'              alternative = "two.one.sided")
#'
#' @export mean.z.test
mean.z.test <- function(power = 0.80, mean = NULL, sign = "+",
                        sd = 1, null.mean = 0, null.sd = 1,
                        alpha = 0.05, alternative = c("two.sided", "one.sided", "two.one.sided"),
                        plot = TRUE, verbose = 1, utf = FALSE) {
  
  alternative <- tolower(match.arg(alternative))
  
  if(power > 0.99) stop("Power cannot be larger than 0.99", call. = FALSE)
  
  if(!is.null(mean)) 
    stop("'mean' should remain NULL", call. = FALSE)
  
  min.null <- stats::qnorm(alpha, mean = min(null.mean), sd = sd)
  min <- stats::qnorm(1e-10, mean = min.null, sd = sd)
  
  max.null <- stats::qnorm(1 - alpha, mean = max(null.mean), sd = sd)
  max <- stats::qnorm(1 - 1e-10, mean = max.null, sd = sd)
  
  if(sign %in% c("-", -1, "-1", "negative")) max <- min(null.mean)
  if(sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) min <- max(null.mean)
  if(sign %in% c(" ", 0, "0", "")) {max <- max(null.mean); min <- min(null.mean)}
  
  mean <- optimize(
    f = function(mean) {
      (power - power.z.test(mean = mean, sd = sd, 
                            null.mean = null.mean, null.sd = null.sd,
                            alpha = alpha, alternative = alternative,
                            plot = FALSE, verbose = 0, utf = FALSE)$power)^2
    },
    maximum = FALSE,
    lower = min,
    upper = max,
  )$minimum
  
  power.z.test(mean = mean, sd = sd,  
                      null.mean = null.mean, null.sd = null.sd,
                      alpha = alpha, alternative = alternative,
                      plot = plot, verbose = verbose, utf = utf)
  
} # mean.z.test

mean.z <- mean.z.test
