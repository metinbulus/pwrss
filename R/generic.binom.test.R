#' Power Analysis for the Generic Binomial Test
#'
#' @description
#' Calculates power for the generic binomial test with (optional) Type 1 and
#' Type 2 error plots.
#'
#' @aliases power.binom
#'
#'
#' @param size        number of trials (zero or more).
#' @param prob        probability of success on each trial under alternative.
#' @param null.prob   probability of success on each trial under null.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided", "one.sided", or "two.one.sided". For
#'                    non-inferiority or superiority tests, add or subtract the
#'                    margin from the null hypothesis value and use alternative
#'                    = "one.sided".
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
#'   \item{size}{number of trials (zero or more).}
#'   \item{prob}{probability of success on each trial under alternative.}
#'   \item{null.prob}{probability of success on each trial under null.}
#'   \item{binom.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # one-sided
#' power.binom.test(size = 200, prob = 0.6, null.prob = 0.5,
#'                  alpha = 0.05, alternative = "one.sided")
#'
#' # two-sided
#' power.binom.test(size = 200, prob = 0.4, null.prob = 0.5,
#'                  alpha = 0.05, alternative = "two.sided")
#'
#' # equivalence
#' power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6),
#'                  alpha = 0.05, alternative = "two.one.sided")
#'
#' @export power.binom.test
power.binom.test <- function(size,
                             prob,
                             null.prob = 0.5,
                             alpha = 0.05,
                             alternative = c("two.sided", "one.sided", "two.one.sided"),
                             plot = TRUE,
                             verbose = 1,
                             utf = FALSE) {

  alternative <- tolower(match.arg(alternative))

  # size is checked below
  check.proportion(prob)
  null.prob <- check.margins(null.prob, check.proportion, alternative)
  check.proportion(alpha)
  check.logical(plot, utf)
  verbose <- ensure_verbose(verbose)

  if (!isInt(size) || any(size < 0))
    stop("Argument `size` does not have a valid value (integer-like, >= 0, and finite).", call. = FALSE)

  if (alternative == "two.sided") {

    q.low     <- stats::qbinom(alpha / 2, size, null.prob, lower.tail = TRUE)
    q.high    <- stats::qbinom(alpha / 2, size, null.prob, lower.tail = FALSE)
    prob.low  <- stats::pbinom(q.low,     size, null.prob, lower.tail = TRUE)
    prob.high <- stats::pbinom(q.high,    size, null.prob, lower.tail = FALSE)
    q.low[prob.low   > alpha / 2] <- q.low[prob.low   > alpha / 2] - 1
    q.high[prob.high > alpha / 2] <- q.high[prob.high > alpha / 2] + 1
    prob.low <- stats::pbinom(q.low, size, null.prob, lower.tail = TRUE)
    prob.high <- stats::pbinom(q.high, size, null.prob, lower.tail = FALSE)
    approx.alpha <-  prob.low + prob.high
    binom.alpha <- c(q.low, q.high)

    power <- stats::pbinom(q.low,  size, prob, lower.tail = TRUE) +
             stats::pbinom(q.high, size, prob, lower.tail = FALSE)

  } else if (alternative == "one.sided") {

    lower.tail <- prob < null.prob

    q          <- stats::qbinom(alpha, size, null.prob, lower.tail = lower.tail)
    prob.alpha <- stats::pbinom(q,     size, null.prob, lower.tail = lower.tail)
    q[prob.alpha > alpha] <- q[prob.alpha > alpha] + ifelse(lower.tail, -1, 1)
    approx.alpha <- stats::pbinom(q, size, null.prob, lower.tail = lower.tail)
    binom.alpha <- q

    power <- stats::pbinom(q, size, prob, lower.tail = lower.tail)

  } else if (alternative == "two.one.sided" && (prob > min(null.prob) && prob < max(null.prob))) {  # equivalence

    q.low     <- stats::qbinom(alpha,  size, prob = null.prob[1], lower.tail = FALSE)
    q.high    <- stats::qbinom(alpha,  size, prob = null.prob[2], lower.tail = TRUE)
    prob.low  <- stats::pbinom(q.low,  size, prob = null.prob[1], lower.tail = FALSE)
    prob.high <- stats::pbinom(q.high, size, prob = null.prob[2], lower.tail = TRUE)
    q.low[prob.low   > alpha] <- q.low[prob.low   > alpha] + 1
    q.high[prob.high > alpha] <- q.high[prob.high > alpha] - 1
    prob.low  <- stats::pbinom(q.low,  size, prob = null.prob[1], lower.tail = FALSE)
    prob.high <- stats::pbinom(q.high, size, prob = null.prob[2], lower.tail = TRUE)
    approx.alpha <-  (prob.low + prob.high) / 2 # average
    binom.alpha  <- c(q.low, q.high)

    power <- stats::pbinom(q.low, size, prob, lower.tail = FALSE) +
             stats::pbinom(q.high, size, prob, lower.tail = TRUE) - 1
    if (power < 0) power <- 0

  } else if (alternative == "two.one.sided" && (prob < min(null.prob) || prob > max(null.prob))) {  # minimal effect

    q.low     <- stats::qbinom(alpha / 2, size, prob = null.prob[1], lower.tail = TRUE)
    q.high    <- stats::qbinom(alpha / 2, size, prob = null.prob[2], lower.tail = FALSE)
    prob.low  <- stats::pbinom(q.low,     size, prob = null.prob[1], lower.tail = TRUE)
    prob.high <- stats::pbinom(q.high,    size, prob = null.prob[2], lower.tail = FALSE)
    q.low[prob.low   > alpha / 2] <- q.low[prob.low   > alpha / 2] - 1
    q.high[prob.high > alpha / 2] <- q.high[prob.high > alpha / 2] + 1
    prob.low  <- stats::pbinom(q.low,     size, prob = null.prob[1], lower.tail = TRUE)
    prob.high <- stats::pbinom(q.high,    size, prob = null.prob[2], lower.tail = FALSE)
    approx.alpha <- prob.low + prob.high
    binom.alpha  <- c(q.low, q.high)

    power <- stats::pbinom(q.low,  size, prob = prob, lower.tail = TRUE) +
             stats::pbinom(q.high, size, prob = prob, lower.tail = FALSE)

  }

  if (plot)
    suppressWarnings(.plot.binom.t1t2(size = size, prob = prob, null.prob = null.prob, alpha = approx.alpha, alternative = alternative))

  if (verbose > 0) {

    print.obj <- list(test = "Generic Binomial Test",
                      requested = "power",
                      size = size,
                      alpha = approx.alpha,
                      alternative = alternative,
                      prob.alternative = prob,
                      prob.null = null.prob,
                      binom.alpha = binom.alpha,
                      power = power)

    .print.pwrss.binom(print.obj, verbose = verbose, utf = utf)

  } # end of verbose

  invisible(list(size = size, alpha = approx.alpha, alternative = alternative,
                 prob = prob, null.prob = null.prob, binom.alpha = binom.alpha, power = power))

} # power.binom.test()

power.binom <- power.binom.test

#' Find Probability (Non-Centrality) for the Generic Binomial Test
#'
#' @description
#' Finds probability (non-centrality) for the generic binomial test with (optional) Type 1 and
#' Type 2 error plots.
#'
#' @aliases prob.binom
#'
#' @param power       statistical power \eqn{(1-\beta)}.
#' @param size        number of trials (zero or more).
#' @param prob        probability of success on each trial under alternative.
#' @param null.prob   probability of success on each trial under null.
#' @param sign        whether 'prob' is expected to be greater '+1', less than '-1', or within '0' the null.prob' bounds.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided", "one.sided", or "two.one.sided". For
#'                    non-inferiority or superiority tests, add or subtract the
#'                    margin from the null hypothesis value and use alternative
#'                    = "one.sided".
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
#'   \item{size}{number of trials (zero or more).}
#'   \item{prob}{probability of success on each trial under alternative.}
#'   \item{null.prob}{probability of success on each trial under null.}
#'   \item{binom.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'
#' @examples
#' # one-sided
#' prob.binom.test(size = 200, prob = NULL, sign = "+", power = 0.80, null.prob = 0.5,
#'                  alpha = 0.05, alternative = "one.sided")
#'
#' # two-sided
#' prob.binom.test(size = 200, prob = NULL, sign = "+", power = 0.80, null.prob = 0.5,
#'                  alpha = 0.05, alternative = "two.sided")
#'
#' # equivalence
#' prob.binom.test(size = 200, prob = NULL, sign = "0", power = 0.80, null.prob = c(0.4, 0.6),
#'                  alpha = 0.05, alternative = "two.one.sided")
#'
#' @export prob.binom.test
prob.binom.test <- function(power = 0.80, 
                            size,
                            prob = NULL,
                            sign = "+",
                            null.prob = 0.5,
                            alpha = 0.05,
                            alternative = c("two.sided", "one.sided", "two.one.sided"),
                            plot = TRUE,
                            verbose = 1,
                            utf = FALSE) {
  
  alternative <- tolower(match.arg(alternative))
  
  if(power > 0.99) stop("Power cannot be larger than 0.99.", call. = FALSE)
  
  min <- 0.0001
  max <- 0.9999
  
  if(sign %in% c("-", -1, "-1", "negative")) max <- min(null.prob)
  if(sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) min <- max(null.prob)
  if(sign %in% c(" ", 0, "0", "")) {max <- max(null.prob); min <- min(null.prob)}
  
  prob <- optimize(
    f = function(prob) {
      (power - power.binom.test(size = size, prob = prob, null.prob = null.prob,
                                alpha = alpha, alternative = alternative,
                                plot = FALSE, verbose = 0, utf = FALSE)$power)^2
    },
    maximum = FALSE,
    lower = min,
    upper = max,
  )$minimum
  
  power.binom.test(size = size, prob = prob, null.prob = null.prob,
                   alpha = alpha, alternative = alternative,
                   plot = plot, verbose = verbose, utf = utf)
  
} # prob.binom.test
prob.binom <- prob.binom.test
