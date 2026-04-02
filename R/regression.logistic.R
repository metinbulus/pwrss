#' Power Analysis for Logistic Regression Coefficient (Wald's Z-Test)
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test a
#' single coefficient in logistic regression. \code{power.z.logistic()} and
#' \code{power.z.logreg()} are the same functions, as well as
#' \code{pwrss.z.logistic()} and \code{pwrss.z.logreg()}.
#'
#' The distribution of the predictor variable can be one of the following:
#' \code{c("normal", "poisson", "uniform", "exponential", "binomial",
#' "bernouilli", "lognormal")} for Demidenko (2007) procedure but only
#' \code{c("normal", "binomial", "bernouilli")} for Hsieh et al. (1998)
#' procedure. The default parameters for these distributions are:
#'
#' \code{distribution = list(dist = "normal", mean = 0, sd = 1)} \cr
#' \code{distribution = list(dist = "poisson", lambda = 1)} \cr
#' \code{distribution = list(dist = "uniform", min = 0, max = 1)} \cr
#' \code{distribution = list(dist = "exponential", rate = 1)} \cr
#' \code{distribution = list(dist = "binomial", size = 1, prob = 0.50)} \cr
#' \code{distribution = list(dist = "bernoulli", prob = 0.50)} \cr
#' \code{distribution = list(dist = "lognormal", meanlog = 0, sdlog = 1)} \cr
#'
#' Parameters defined in \code{list()} form can be modified, but element names
#' should be kept the same. It is sufficient to use distribution's name for
#' default parameters (e.g. \code{dist = "normal"}).
#'
#' Formulas are validated using G*Power and tables in the PASS documentation.
#'
#' @details
#' * NB: The \code{pwrss.z.logistic()} and its alias \code{pwrss.z.logreg()}
#'   are deprecated. However, they will remain available as wrappers for the
#'   \code{power.z.logistic()} function during a transition period.
#'
#' @aliases power.z.logistic pwrss.z.logistic power.z.logreg pwrss.z.logreg
#'
#'
#' @param base.prob           base probability under null hypothesis
#'                            (probability that an event occurs without the
#'                            influence of the predictor - or when the value of
#'                            the predictor is zero).
#' @param prob                probability under alternative hypothesis
#'                            (probability that an event occurs when the value
#'                            of the predictor is increased from 0 to 1).
#'                            Warning: This is base probability + incremental
#'                            increase.
#' @param beta0               regression coefficient defined as
#'                            \code{beta0 = log(base.prob/(1-base.prob))}
#' @param beta1               regression coefficient for the predictor X defined as
#'                            \code{beta1 = log((prob / (1 - prob)) / (base.prob / (1 - base.prob)))}
#' @param beta1.sign          sign of the beta1 coefficient (when minimum 
#'                            detectable effect or beta1 is of interest).
#' @param odds.ratio          odds ratio defined as
#'                            \code{odds.ratio = exp(beta1) = (prob / (1 - prob)) / (base.prob / (1 - base.prob))}
#' @param n                   integer; sample size
#' @param power               statistical power, defined as the probability of
#'                            correctly rejecting a false null hypothesis,
#'                            denoted as \eqn{1 - \beta}.
#' @param r.squared.predictor proportion of variance in the predictor accounted
#'                            for by other covariates. This is not a pseudo
#'                            R-squared. To compute it, regress the predictor
#'                            on the covariates and extract the adjusted
#'                            R-squared from that model.
#' @param alpha               type 1 error rate, defined as the probability of
#'                            incorrectly rejecting a true null hypothesis,
#'                            denoted as \eqn{\alpha}.
#' @param alternative         character; the direction or type of the
#'                            hypothesis test: "two.sided" or "one.sided".
#' @param method              character; analytic method.
#'                            \code{"demidenko(vc)"} stands for Demidenko
#'                            (2007) procedure with variance correction;
#'                            \code{"demidenko"} stands for Demidenko (2007)
#'                            procedure without variance correction;
#'                            \code{"hsieh"} stands for Hsieh et al. (1998)
#'                            procedure. \code{"demidenko"} and \code{"hsieh"}
#'                            methods produce similar results but
#'                            \code{"demidenko(vc)"} is more precise.
#' @param distribution        character; distribution family. Can be one of the
#'                            \code{c("normal", "poisson", "uniform",
#'                            "exponential", "binomial", "bernouilli",
#'                            "lognormal")} for Demidenko (2007) procedure but
#'                            only \code{c("normal", "binomial", "bernouilli")}
#'                            for the Hsieh et al. (1998) procedure.
#' @param ceiling             logical; whether sample size should be rounded
#'                            up. \code{TRUE} by default.
#' @param verbose             \code{1} by default (returns test, hypotheses,
#'                            and results), if \code{2} a more detailed output
#'                            is given (plus key parameters and definitions), if
#'                            \code{0} no output is printed on the console.
#' @param utf                 logical; whether the output should show Unicode
#'                            characters (if encoding allows for it).
#'                            \code{FALSE} by default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (Z-Test).}
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{z.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{n}{sample size.}
#'
#' @references
#'   Demidenko, E. (2007). Sample size determination for logistic regression
#'   revisited. *Statistics in Medicine, 26*(18), 3385-3397.
#'   https://doi.org/10.1002/sim.2771
#'
#'   Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of
#'   sample size calculation for linear and logistic regression. *Statistics in
#'   Medicine, 17*(4), 1623-1634.
#'
#' @examples
#' ###########################################
#' # predictor X follows normal distribution #
#' ###########################################
#'
#' ## probability specification
#' power.z.logistic(base.prob = 0.15, prob = 0.20,
#'                  alpha = 0.05, power = 0.80,
#'                  dist = "normal")
#'
#' ## odds ratio specification
#' power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
#'                  alpha = 0.05, power = 0.80,
#'                  dist = "normal")
#'
#' ## regression coefficient specification
#' power.z.logistic(beta0 = -1.734601, beta1 = 0.3483067,
#'                  alpha = 0.05, power = 0.80,
#'                  dist = "normal")
#'
#' ## change parameters associated with predictor X
#' pred.dist <- list(dist = "normal", mean = 10, sd = 2)
#' power.z.logistic(base.prob = 0.15, beta1 = 0.3483067,
#'                  alpha = 0.05, power = 0.80,
#'                  dist = pred.dist)
#'
#'
#' ##############################################
#' # predictor X follows Bernoulli distribution #
#' # (such as treatment/control groups)         #
#' ##############################################
#'
#' ## odds ratio specification
#' power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
#'                  alpha = 0.05, power = 0.80,
#'                  dist = "bernoulli")
#'
#' ## change parameters associated with predictor X
#' pred.dist <- list(dist = "bernoulli", prob = 0.30)
#' power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
#'                  alpha = 0.05, power = 0.80,
#'                  dist = pred.dist)
#'
#' ####################################
#' # predictor X is an ordinal factor #
#' ####################################
#'
#' ## generating an ordinal predictor
#' x.ord <- sample(
#'   x = c(1, 2, 3, 4), # levels
#'   size = 1e5, # sample size large enough to get stable estimates
#'   prob = c(0.25, 0.25, 0.25, 0.25), # category probabilities
#'   replace = TRUE
#' )
#'
#' ## dummy coding the ordinal predictor
#' x.ord <- factor(x.ord, ordered = TRUE)
#' contrasts(x.ord) <- contr.treatment(4, base = 4)
#' x.dummy <- model.matrix( ~ x.ord)[,-1]
#' x.data <- as.data.frame(x.dummy)
#'
#' ## fit linear regression to get multiple r-squared
#' x.fit <- lm(x.ord1 ~ x.ord2 + x.ord3, data = x.data)
#'
#' ## extract parameters
#' bern.prob <- mean(x.data$x.ord1)
#' r.squared.pred <- summary(x.fit)$adj.r.squared
#'
#' ## change parameters associated with predictor X
#' pred.dist <- list(dist = "bernoulli", prob = bern.prob)
#' power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
#'                alpha = 0.05, power = 0.80,
#'                r.squared.pred = r.squared.pred,
#'                dist = pred.dist)
#'
#' @export power.z.logistic
power.z.logistic <- function(prob = NULL, base.prob = NULL, odds.ratio = NULL,
                             beta0 = NULL, beta1 = NULL, beta1.sign = "+",
                             n = NULL, power = NULL,
                             r.squared.predictor = 0,
                             alpha = 0.05, alternative = c("two.sided", "one.sided"),
                             method = c("demidenko(vc)", "demidenko", "hsieh"),
                             distribution = "normal", ceiling = TRUE,
                             verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  method <- tolower(match.arg(method))
  func.parms <- clean.parms(as.list(environment()))

  check.proportion(r.squared.predictor)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  # requested <- check.n_power(n, power)
  if(is.null(n)) requested <- "n"
  if(is.null(power)) requested <- "power"
  if(is.null(prob) & is.null(beta1) & is.null(odds.ratio)) requested <- "es"

  if (all(check.not_null(base.prob, prob))) {
    check.proportion(prob, base.prob)
    if (any(check.not_null(odds.ratio, beta0, beta1)) && verbose >= 0)
      message("Using `base.prob` and `prob`, ignoring any specifications to `odds.ratio`, `beta0`, or `beta1`.")
    if (prob == base.prob) 
      stop("`prob` can not have the same value as `base.prob`.", call. = FALSE)
    odds.ratio <- (prob / (1 - prob)) / (base.prob / (1 - base.prob))
    beta0 <- log(base.prob / (1 - base.prob))
    beta1 <- log(odds.ratio)
  } else if (all(check.not_null(base.prob, odds.ratio))) {
    check.proportion(base.prob)
    check.positive(odds.ratio)
    if (any(check.not_null(prob, beta0, beta1)) && verbose >= 0)
      message("Using `base.prob` and `odds.ratio`, ignoring any specifications to `prob`, `beta0`, or `beta1`.")
    prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
    if (prob == base.prob) 
      stop("`prob` can not have the same value as `base.prob`.", call. = FALSE)
    beta0 <- log(base.prob / (1 - base.prob))
    beta1 <- log(odds.ratio)
  } else if (all(check.not_null(base.prob, beta1))) {
    check.proportion(base.prob)
    check.numeric(beta1)
    if (any(check.not_null(prob, beta0, odds.ratio)) && verbose >= 0)
      message("Using `base.prob` and `beta1`, ignoring any specifications to `prob`, `beta0`, or `odds.ratio`.")
    odds.ratio <- exp(beta1)
    prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
    if (prob == base.prob) 
      stop("`prob` can not have the same value as `base.prob`.", call. = FALSE)
    beta0 <- log(base.prob / (1 - base.prob))
  } else if (all(check.not_null(beta0, beta1))) {
    check.numeric(beta0, beta1)
    if (any(check.not_null(base.prob, prob, odds.ratio)) && verbose >= 0)
      message("Using `beta0` and `beta1`, ignoring any specifications to `base.prob`, `prob`, or `odds.ratio`.")
    if (beta0 == beta1) 
      stop("`beta1` can not have the same value as `beta0`.", call. = FALSE)
    base.prob <- exp(beta0) / (1 + exp(beta0))
    odds.ratio <- exp(beta1)
    prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
  } else {
    if(is.null(n) || is.null(power)) 
      stop("Specify `base.prob` & `prob` \n  or `base.prob` & `odds.ratio` \n  or `base.prob` & `beta1`\n  or `beta0` & `beta1`.", call. = FALSE)
    if(requested == "es") {
      if(is.null(base.prob) & !is.null(beta0)) {
        base.prob <- exp(beta0) / (1 + exp(beta0))
      } else if(!is.null(base.prob) & is.null(beta0)) {
        beta0 <- log(base.prob / (1 - base.prob))
      } else {
        stop("Provide 'base.prob' or 'beta0'", call. = FALSE)
      }
    }
  }
  
  # check distribution
  if (length(distribution) == 1 && is.character(distribution)) {
    distribution <- switch(tolower(distribution),
                           `normal` = list(dist = "normal", mean = 0, sd = 1),
                           `poisson` = list(dist = "poisson", lambda = 1),
                           `uniform` = list(dist = "uniform", min = 0, max = 1),
                           `exponential` = list(dist = "exponential", rate = 1),
                           `binomial` = list(dist = "binomial", size = 1, prob = 0.50),
                           `bernoulli` = list(dist = "bernoulli", prob = 0.50),
                           `lognormal` = list(dist = "lognormal", meanlog = 0, sdlog = 1))
  } else if (is.list(distribution)) {
    if (length(distribution) > 3) stop("Unknown input type for `distribution`.", call. = FALSE)
    dist.list.names <- names(distribution)
    dist.attrib <- c(dist.list.names, tolower(distribution$dist))
    dist.invalid <- c(any(is.na(match(dist.attrib, c("dist", "normal", "mean", "sd")))),
                      any(is.na(match(dist.attrib, c("dist", "poisson", "lambda")))),
                      any(is.na(match(dist.attrib, c("dist", "uniform", "min", "max")))),
                      any(is.na(match(dist.attrib, c("dist", "exponential", "rate")))),
                      any(is.na(match(dist.attrib, c("dist", "binomial", "size", "prob")))),
                      any(is.na(match(dist.attrib, c("dist", "bernoulli", "prob")))),
                      any(is.na(match(dist.attrib, c("dist", "lognormal", "meanlog", "sdlog")))))
    if (all(dist.invalid == TRUE)) stop("Unknown input type for `distribution`.", call. = FALSE)
  } else {
    stop("Unknown input type for `distribution`.", call. = FALSE)
  }

  # asymptotic variances
  var.beta <- function(beta0, beta1, distribution) {
    prec <- 1e-8

    if (tolower(distribution$dist) == "normal") {

      min <- stats::qnorm(prec,     mean = distribution$mean, sd = distribution$sd)
      max <- stats::qnorm(1 - prec, mean = distribution$mean, sd = distribution$sd)

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dnorm(x, mean = distribution$mean, sd = distribution$sd)
      calcInt <- FALSE

    }  else if (tolower(distribution$dist) == "poisson") {

      min <- 0
      max <- stats::qpois(1 - prec, lambda = distribution$lambda)

      # define the distribution function and use summation (calcInt == TRUE)
      dist.func <- function(x) stats::dpois(x, lambda = distribution$lambda)
      calcInt <- TRUE

    }  else if (tolower(distribution$dist) == "uniform") {

      min <- distribution$min
      max <- distribution$max

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dunif(x, min = min, max = max)
      calcInt <- FALSE

    } else if (tolower(distribution$dist) == "exponential") {

      min <- 0
      max <- stats::qexp(1 - prec, rate = distribution$rate)

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dexp(x, rate = distribution$rate)
      calcInt <- FALSE

    }  else if (tolower(distribution$dist) %in% c("binomial", "bernoulli")) {

      min  <- 0
      max  <- ifelse(tolower(distribution$dist) == "bernoulli", 1, distribution$size)

      # define the distribution function and use summation (calcInt == TRUE)
      dist.func <- function(x) stats::dbinom(x, size = max, prob = distribution$prob)
      calcInt <- TRUE

    } else if (tolower(distribution$dist) == "lognormal") {

      min <- stats::qlnorm(prec,     meanlog = distribution$meanlog, sdlog = distribution$sdlog)
      max <- stats::qlnorm(1 - prec, meanlog = distribution$meanlog, sdlog = distribution$sdlog)

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dlnorm(x, meanlog = distribution$meanlog, sdlog = distribution$sdlog)
      calcInt <- FALSE

    } # log-normal

    # carry out the actual calculations
    # [1] define the variance function
    var.func <- function(x, e1, b0, b1, e2) x ^ e1 * dist.func(x) * exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)) ^ e2

    # [2A] use summation to “integrate” integer sequences OR
    if (calcInt) {

      # determine which sequence should be summed up
      calc.seq <- seq(min, max)

      # for mu: e1 [first parm.] = 0 -> x ^ e1 == 1, the log of which is beta0* (beta0s)
      # calculate mu and beta0s -                     | parms. to var.func
      mu  <- sum(vapply(calc.seq, var.func, numeric(1), 0, beta0,  beta1, 1), na.rm = TRUE)
      beta0s <- log(mu / (1 - mu))

      # variance under null -                         | parms. to var.func
      i00 <- sum(vapply(calc.seq, var.func, numeric(1), 0, beta0s, 0,     2), na.rm = TRUE)
      i01 <- sum(vapply(calc.seq, var.func, numeric(1), 1, beta0s, 0,     2), na.rm = TRUE)
      i11 <- sum(vapply(calc.seq, var.func, numeric(1), 2, beta0s, 0,     2), na.rm = TRUE)
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative -                  | parms. to var.func
      i00 <- sum(vapply(calc.seq, var.func, numeric(1), 0, beta0,  beta1, 2), na.rm = TRUE)
      i01 <- sum(vapply(calc.seq, var.func, numeric(1), 1, beta0,  beta1, 2), na.rm = TRUE)
      i11 <- sum(vapply(calc.seq, var.func, numeric(1), 2, beta0,  beta1, 2), na.rm = TRUE)
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    # [2B] use integration for real numbers
    } else {

      # for mu: e1 [first parm.] = 0 -> x ^ e1 == 1, the log of which is beta0* (beta0s)
      # calculate mu and beta0s -               | parms. to var.func
      mu  <- stats::integrate(var.func, min, max, 0, beta0,  beta1, 1)$value
      beta0s <- log(mu / (1 - mu))

      # variance under null -                   | parms. to var.func
      i00 <- stats::integrate(var.func, min, max, 0, beta0s, 0,     2)$value
      i01 <- stats::integrate(var.func, min, max, 1, beta0s, 0,     2)$value
      i11 <- stats::integrate(var.func, min, max, 2, beta0s, 0,     2)$value
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative -            | parms. to var.func
      i00 <- stats::integrate(var.func, min, max, 0, beta0,  beta1, 2)$value
      i01 <- stats::integrate(var.func, min, max, 1, beta0,  beta1, 2)$value
      i11 <- stats::integrate(var.func, min, max, 2, beta0,  beta1, 2)$value
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    }

    list(var.beta0 = var.beta0, var.beta1 = var.beta1,
         distribution = tolower(distribution$dist),
         min = min, max = max)

  } # var.beta


  # Demidenko, E. (2007). Sample size determination for logistic
  # regression revisited. Statistics in Medicine, 26, 3385-3397.
  pwr.demidenko <- function(beta0, beta1, n,
                            r.squared.predictor, alpha, alternative,
                            method, distribution) {

    # variance correction factor
    if (method == "demidenko(vc)") {
      vcf <- switch(tolower(distribution$dist),
                    `normal` = 1,
                    `poisson` = 1,
                    `uniform` = 1,
                    `exponential` = 1,
                    `binomial` = 0.85,
                    `bernoulli` = 0.85,
                    `lognormal` = 0.75)
    } else if (tolower(method) == "demidenko") {
      vcf <- 0
    }

    var.obj <- var.beta(beta0 = beta0, beta1 = beta1, distribution = distribution)
    var.beta0 <- var.obj$var.beta0
    var.beta1 <- var.obj$var.beta1

    # non-centrality parameter and standard deviation of the non-centrality parameter under alternative
    ncp <- beta1 / sqrt(var.beta1 / (n * (1 - r.squared.predictor)))
    sd.ncp <- sqrt((vcf * var.beta0 + (1 - vcf) * var.beta1) / var.beta1)

    pwr.obj <- power.z.test(mean = ncp, sd = sd.ncp, null.mean = 0,
                            alpha = alpha, alternative = alternative,
                            plot = FALSE, verbose = 0)
    power <- pwr.obj$power
    z.alpha <- pwr.obj$z.alpha

    list(power = power, ncp = ncp, sd.ncp = sd.ncp, vcf = vcf, z.alpha = z.alpha)

  } # pwr.demidenko()

  ss.demidenko <- function(beta0, beta1, power,
                           r.squared.predictor, alpha, alternative,
                           method, distribution) {

    n <- stats::uniroot(function(n) {
      power - pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n,
                            r.squared.predictor = r.squared.predictor,
                            alpha = alpha, alternative = alternative,
                            method = method, distribution = distribution)$power
    }, interval = c(2, 1e10))$root

    n

  } # ss.demidenko()
  
  es.demidenko <- function(beta0, beta1.sign, n, power, 
                           r.squared.predictor,
                           alpha, alternative,
                           method, distribution) {
    
    # reasonable bounds for logistics
    var.obj <- var.beta(beta0 = beta0, beta1 = beta0, distribution = distribution)
    min.x <- var.obj$min 
    max.x <- var.obj$max 
    
    bound.values <- c((qlogis(0.0001) - beta0) / c(min.x, max.x), 
                      (qlogis(0.9999) - beta0) / c(min.x, max.x))
    
    beta1.min <- min(bound.values)
    beta1.max <- max(bound.values)
    
    if(beta1.sign %in% c("-", -1, "-1", "negative")) {
      beta1 <- try({
        stats::uniroot(function(beta1) {
          power - pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n,
                                r.squared.predictor = r.squared.predictor,
                                alpha = alpha, alternative = alternative,
                                method = method, distribution = distribution)$power
        }, interval = c(beta1.min, 0))$root
      })
      
      if(inherits(beta1, "try-error")) 
        stop("Design is not feasible. Try 'beta1.sign = '+'", call. = FALSE)
      
    } # negative
    
    if(beta1.sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) {
      beta1 <-  try({
        stats::uniroot(function(beta1) {
          power - pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n,
                                r.squared.predictor = r.squared.predictor,
                                alpha = alpha, alternative = alternative,
                                method = method, distribution = distribution)$power
        }, interval = c(0, beta1.max))$root
      })
    
      if(inherits(beta1, "try-error")) 
        stop("Design is not feasible. Try 'beta1.sign = '-'", call. = FALSE)
      
    } # positive
    
    return(beta1)
    
  } # es.demidenko()
  
  # Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple
  # method of sample size calculation for linear and logistic
  # regression. Statistics in Medicine, 17, 1623-1634.
  ss.hsieh <- function(base.prob, prob,
                       r.squared.predictor,
                       power, alpha, alternative,
                       distribution) {

    if (tolower(distribution$dist) %in% c("binomial", "bernoulli")) {

      if (tolower(distribution$dist) == "binomial" && distribution$size > 1)
        stop(paste("Hsieh et al. (1998) is valid only for a binary covariate or a continuous covariate following normal",
                   "distribution."), call. = FALSE)
      dist.prob <- distribution$prob
      z.alpha <- stats::qnorm(ifelse(alternative == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
      z.beta  <- stats::qnorm(1 - power, lower.tail = FALSE)
      p.bar <- (1 - dist.prob) * base.prob + dist.prob * prob
      beta1 <- log((prob / (1 - prob)) / (base.prob / (1 - base.prob)))
      n <- (z.alpha * sqrt(p.bar * (1 - p.bar) / dist.prob) +
            z.beta  * sqrt(base.prob * (1 - base.prob) + prob * (1 - prob) * (1 - dist.prob) / dist.prob)) ^ 2 /
            ((base.prob - prob) ^ 2 * (1 - dist.prob))
      n <- n / (1 - r.squared.predictor)

    } else if (tolower(distribution$dist) == "normal") {

      beta <- 1 - power
      z.alpha <- stats::qnorm(ifelse(alternative == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
      z.beta <- stats::qnorm(beta, lower.tail = FALSE)
      odds.ratio <- (prob / (1 - prob)) / (base.prob / (1 - base.prob))
      beta1 <- log(odds.ratio)
      n <- (z.alpha + z.beta) ^ 2 / (base.prob * (1 - base.prob) * beta1 ^ 2)
      n <- n / (1 - r.squared.predictor)

    } else {

      stop("Not a valid distribution for the Hsieh et al. (1998) procedure.", call. = FALSE)

    }

    list(n = n, ncp = z.alpha + z.beta, sd.ncp = 1, vcf = NA, z.alpha = z.alpha)

  } # ss.hsieh()


  pwr.hsieh <- function(base.prob, prob,
                       r.squared.predictor,
                       n, alpha, alternative,
                       distribution) {

    power <- stats::uniroot(function(power) {
      n - ss.hsieh(base.prob = base.prob, prob = prob,
                   r.squared.predictor = r.squared.predictor,
                   power = power, alpha = alpha,
                   alternative = alternative,
                   distribution = distribution)$n
    }, interval = c(1e-3, 1 - 1e-3))$root

    power

  } # pwr.hsieh
  
  es.hsieh <- function(base.prob, beta1.sign, n, power, 
                       r.squared.predictor,
                       alpha, alternative,
                       distribution) {
    
    # reasonable bounds for prob
    prob.min <- 0.0001
    prob.max <- 0.9999
    
    if(beta1.sign %in% c("-", -1, "-1", "negative")) {
      prob <- try({
        stats::uniroot(function(prob) {
          n - ss.hsieh(base.prob = base.prob, prob = prob,
                       r.squared.predictor = r.squared.predictor,
                       power = power, alpha = alpha,
                       alternative = alternative,
                       distribution = distribution)$n
        }, interval = c(prob.min, base.prob))$root
      })
      
      if(inherits(prob, "try-error")) 
        stop("Design is not feasible. Try 'beta1.sign = '+'", call. = FALSE)
      
    } # negative
    
    if(beta1.sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) {
      prob <- try({
        stats::uniroot(function(prob) {
          n - ss.hsieh(base.prob = base.prob, prob = prob,
                       r.squared.predictor = r.squared.predictor,
                       power = power, alpha = alpha,
                       alternative = alternative,
                       distribution = distribution)$n
        }, interval = c(base.prob, prob.max))$root
      })
      
      if(inherits(prob, "try-error")) 
        stop("Design is not feasible. Try 'beta1.sign = '-'", call. = FALSE)
      
    } # positive
    
    return(prob)
    
  } # es.hsieh()


  if (method %in% c("demidenko(vc)", "demidenko")) {

    if (requested == "n") {

      n <- ss.demidenko(beta0 = beta0, beta1 = beta1, power = power,
                        r.squared.predictor = r.squared.predictor,
                        alpha = alpha, alternative = alternative,
                        method = method, distribution = distribution)

      if (ceiling) n <- ceiling(n)

    }
    
    if (requested == "es") {
      
      beta1 <- es.demidenko(beta0 = beta0, beta1.sign = beta1.sign, 
                            n = n, power = power,
                            r.squared.predictor = r.squared.predictor,
                            alpha = alpha, alternative = alternative,
                            method = method, distribution = distribution)
      
      base.prob <- exp(beta0) / (1 + exp(beta0))
      odds.ratio <- exp(beta1)
      prob <- odds.ratio * (base.prob / (1 - base.prob)) / (1 + odds.ratio * (base.prob / (1 - base.prob)))
 
    }
    
   
    # calculate power (if requested == "power") or update it (if requested == "n")
    pwr.obj <- pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n, r.squared.predictor = r.squared.predictor,
                             alpha = alpha, alternative = alternative, method = method, distribution = distribution)

    power <- pwr.obj$power
    z.alpha <- pwr.obj$z.alpha
    ncp <- pwr.obj$ncp
    sd.ncp <- pwr.obj$sd.ncp
    vcf <- pwr.obj$vcf

  } else if (method == "hsieh") {

    if (requested == "n") {

      n <- ss.hsieh(base.prob = base.prob, prob = prob, r.squared.predictor = r.squared.predictor,
                    power = power, alpha = alpha, alternative = alternative, distribution = distribution)$n

      if (ceiling) n <- ceiling(n)

    }
    
    if (requested == "es") {
      
     prob <- es.hsieh(base.prob = base.prob, 
                        beta1.sign = beta1.sign,
                        n = n, power = power, 
                        r.squared.predictor = r.squared.predictor,
                        alpha = alpha, alternative = alternative,
                        distribution = distribution)
      
     odds.ratio <- (prob / (1 - prob)) / (base.prob / (1 - base.prob))
     beta0 <- log(base.prob / (1 - base.prob))
     beta1 <- log(odds.ratio)
      
    }

    # calculate power (if requested == "power") or update it (if requested == "n")
    power <- pwr.hsieh(base.prob = base.prob, prob = prob, r.squared.predictor = r.squared.predictor, n = n,
                       alpha = alpha, alternative = alternative, distribution = distribution)

    ss.obj <- ss.hsieh(base.prob = base.prob, prob = prob, r.squared.predictor = r.squared.predictor, power = power,
                       alpha = alpha, alternative = alternative, distribution = distribution)

    z.alpha <- ss.obj$z.alpha
    ncp <- ss.obj$ncp
    sd.ncp <- ss.obj$sd.ncp
    vcf <- ss.obj$vcf

  } # method

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = "Logistic Regression Coefficient (Wald's Z-Test)",
                      method = switch(method,
                                      `demidenko(vc)` = "Demidenko (Variance Corrected)",
                                      `demidenko` = "Demidenko",
                                      `hsieh` = "Hsieh"),
                      dist = switch(tolower(distribution$dist),
                                    `normal` = "Normal",
                                    `poisson` = "Poisson",
                                    `bernoulli` = "Bernoulli",
                                    `binomial` = "Binomial",
                                    `lognormal` = "Log-normal",
                                    `uniform` = "Uniform",
                                    `exponential` = "Exponential"),
                      alternative = alternative,
                      base.prob = base.prob,
                      odds.ratio = odds.ratio,
                      n = n,
                      mean.alternative = ncp,
                      sd.alternative = sd.ncp,
                      vcf = vcf,
                      mean.null = 0,
                      sd.null = 1,
                      z.alpha = z.alpha,
                      alpha = alpha,
                      power = power)

    .print.pwrss.logistic(print.obj, verbose = verbose, utf = utf)

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           odds.ratio = odds.ratio,
                           mean = ncp,
                           sd = sd.ncp,
                           vcf = vcf,
                           null.mean = 0,
                           null.sd = 1,
                           z.alpha = z.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "z", "logistic")))
} # end of power.z.logistic()

#' @export power.z.logreg
power.z.logreg <- power.z.logistic


#' @export pwrss.z.logistic
pwrss.z.logistic <- function(p1 = NULL, p0 = NULL, odds.ratio  = NULL,
                             beta0 = NULL, beta1 = NULL,
                             n = NULL, power = NULL, r2.other.x = 0,
                             alpha = 0.05, alternative = c("not equal", "less", "greater"),
                             method = c("demidenko(vc)", "demidenko", "hsieh"),
                             distribution = "normal", verbose = TRUE) {

  method <- tolower(match.arg(method))
  alternative <- tolower(match.arg(alternative))
  verbose <- ensure_verbose(verbose)

  if (alternative %in% c("less", "greater")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"

  logreg.obj <- power.z.logistic(prob = p1, base.prob = p0, odds.ratio = odds.ratio,
                                 beta0 = beta0, beta1 = beta1, n = n, power = power,
                                 r.squared.predictor = r2.other.x, alpha = alpha,
                                 alternative = alternative, method = method,
                                 distribution = distribution, ceiling = TRUE,
                                 verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.logistic() function. \n")

  invisible(logreg.obj)

} # pwrss.z.logistic

#' @export pwrss.z.logreg
pwrss.z.logreg <- pwrss.z.logistic
