#' Power Analysis for Poisson Regression Coefficient (Wald's z Test)
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test a
#' single coefficient in poisson regression. \code{power.z.poisson()} and
#' \code{power.z.poisreg()} are the same functions, as well as
#' \code{pwrss.z.poisson()} and \code{pwrss.z.poisreg()}. The distribution of
#' the predictor variable can be one of the following: \code{c("normal",
#' "poisson", "uniform", "exponential", "binomial", "bernouilli",
#' "lognormal")}. The default parameters for these distributions are
#'
#' \code{distribution = list(dist = "normal", mean = 0, sd = 1)} \cr
#' \code{distribution = list(dist = "poisson", lambda = 1)} \cr
#' \code{distribution = list(dist = "uniform", min = 0, max = 1)} \cr
#' \code{distribution = list(dist = "exponential", rate = 1)} \cr
#' \code{distribution = list(dist = "binomial", size = 1, prob = 0.50)} \cr
#' \code{distribution = list(dist = "bernoulli", prob = 0.50)} \cr
#' \code{distribution = list(dist = "lognormal", meanlog = 0, sdlog = 1)} \cr
#'
#' Parameters defined in \code{list()} form can be modified, but the names
#' should be kept the same. It is sufficient to use distribution's name for
#' default parameters (e.g. \code{dist = "normal"}).
#'
#' Formulas are validated using Monte Carlo simulation, G*Power, and tables in
#' the PASS documentation.
#'
#' @details
#' * NB: The \code{pwrss.z.poisson()} and its alias \code{pwrss.z.poisreg()}
#'   are deprecated. However, they will remain available as wrappers for the
#'   \code{power.z.logistic()} function during a transition period.
#'
#' @aliases power.z.poisreg power.z.poisson pwrss.z.poisreg pwrss.z.poisson
#'
#'
#' @param base.rate           the base mean event rate.
#' @param rate.ratio          event rate ratio. The relative increase in the
#'                            mean event rate for one unit increase in the
#'                            predictor (similar to odds ratio in logistic
#'                            regression).
#' @param beta0               the natural logarithm of the base mean event
#'                            rate.
#' @param beta1               the natural logarithm of the relative increase
#'                            in the mean event rate for one unit increase in
#'                            the predictor.
#' @param beta1.sign          sign of the beta1 coefficient (when minimum 
#'                            detectable effect or beta1 is of interest).
#' @param mean.exposure       the mean exposure time (should be > 0), usually
#'                            it is 1.
#' @param n                   integer; sample size.
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
#' @param method              character; calculation method:
#'                            \code{"demidenko(vc)"} stands for Demidenko
#'                            (2007) procedure with variance correction;
#'                            \code{"demidenko"} stands for Demidenko (2007)
#'                            procedure without variance correction;
#'                            \code{"signorini"} stands for Signorini (1991)
#'                            procedure. \code{"demidenko"} and
#'                            \code{"signorini"} methods produce similar
#'                            results but \code{"demidenko(vc)"} is more
#'                            precise.
#' @param distribution        character; distribution family. Can be one of the
#'                            \code{c("normal", "poisson", "uniform",
#'                            "exponential", "binomial", "bernouilli",
#'                            "lognormal")}.
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
#'   \item{power}{statistical power \eqn{(1-\beta)}}
#'   \item{n}{sample size.}
#'
#' @references
#'   Demidenko, E. (2007). Sample size determination for logistic regression
#'   revisited. *Statistics in Medicine, 26*(18), 3385-3397.
#'   https://doi.org/10.1002/sim.2771
#'
#'   Signorini, D. F. (1991). Sample size for Poisson regression. *Biometrika,
#'   78*(2), 446-450.
#'
#' @examples
#' # predictor X follows normal distribution
#'
#' ## regression coefficient specification
#' power.z.poisson(beta0 = 0.50, beta1 = -0.10,
#'                 alpha = 0.05, power = 0.80,
#'                 dist = "normal")
#'
#' ## rate ratio specification
#' power.z.poisson(base.rate = exp(0.50),
#'                 rate.ratio = exp(-0.10),
#'                 alpha = 0.05, power = 0.80,
#'                 dist = "normal")
#'
#' ## change parameters associated with predictor X
#' dist.x <- list(dist = "normal", mean = 10, sd = 2)
#' power.z.poisson(base.rate = exp(0.50),
#'                 rate.ratio = exp(-0.10),
#'                 alpha = 0.05, power = 0.80,
#'                 dist = dist.x)
#'
#'
#' # predictor X follows Bernoulli distribution (such as treatment/control groups)
#'
#' ## regression coefficient specification
#' power.z.poisson(beta0 = 0.50, beta1 = -0.10,
#'                 alpha = 0.05, power = 0.80,
#'                 dist = "bernoulli")
#'
#' ## rate ratio specification
#' power.z.poisson(base.rate = exp(0.50),
#'                 rate.ratio = exp(-0.10),
#'                 alpha = 0.05, power = 0.80,
#'                 dist = "bernoulli")
#'
#' ## change parameters associatied with predictor X
#' dist.x <- list(dist = "bernoulli", prob = 0.30)
#' power.z.poisson(base.rate = exp(0.50),
#'                 rate.ratio = exp(-0.10),
#'                 alpha = 0.05, power = 0.80,
#'                 dist = dist.x)
#'
#' @export power.z.poisson
power.z.poisson <- function(base.rate = NULL, rate.ratio = NULL,
                            beta0 = NULL, beta1 = NULL, 
                            beta1.sign = NULL,
                            n = NULL, power = NULL,
                            r.squared.predictor = 0, mean.exposure = 1,
                            alpha = 0.05, alternative = c("two.sided", "one.sided"),
                            method = c("demidenko(vc)", "demidenko", "signorini"),
                            distribution = "normal", ceiling = TRUE,
                            verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  method <- tolower(match.arg(method))
  func.parms <- clean.parms(as.list(environment()))

  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(r.squared.predictor)
  check.positive(mean.exposure)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  
  # requested <- check.n_power(n, power)
  if(is.null(n)) requested <- "n"
  if(is.null(power)) requested <- "power"
  if(is.null(beta1) & is.null(rate.ratio)) requested <- "es"
  
  if (all(check.not_null(base.rate, rate.ratio))) {
    if (any(check.not_null(beta0, beta1)) && verbose >= 0)
      message("Using `base.rate` and `rate.ratio`, ignoring any specifications to `beta0` or `beta1`.")
    check.nonnegative(base.rate, rate.ratio)
    beta0 <- log(base.rate)
    beta1 <- log(rate.ratio)
    if (beta0 == beta1) stop("`beta0` can not have the same value as `beta1`.", call. = FALSE)
  } else if (all(check.not_null(beta0, beta1))) {
    if (any(check.not_null(base.rate, rate.ratio)) && verbose >= 0)
      message("Using `beta0` and `beta1`, ignoring any specifications to `base.rate` or `rate.ratio`.")
    check.numeric(beta0, beta1)
    base.rate <- exp(beta0)
    rate.ratio <- exp(beta1)
    if (base.rate == rate.ratio) stop("`base.rate` can not have the same value as `rate.ratio`.", call. = FALSE)
  } else {
    
    if(is.null(n) || is.null(power)) 
      stop("Specify `base.rate` & `rate.ratio` or\n`beta0` & `beta1`.", call. = FALSE)
    
  }


  if (is.character(distribution) && length(distribution) == 1) {
    distribution <- switch(tolower(distribution),
                            `normal` = list(dist = "normal", mean = 0, sd = 1),
                            `poisson` = list(dist = "poisson", lambda = 1),
                            `uniform` = list(dist = "uniform", min = 0, max = 1),
                            `exponential` = list(dist = "exponential", rate = 1),
                            `binomial` = list(dist = "binomial", size = 1, prob = 0.50),
                            `bernoulli` = list(dist = "bernoulli", prob = 0.50),
                            `lognormal` = list(dist = "lognormal", meanlog = 0, sdlog = 1))
  } else if (is.list(distribution)) {
    if (length(distribution) > 3) stop("Unknown input type for `distribution` argument", call. = FALSE)
    dist.list.names <- names(distribution)
    dist.attrib <- c(dist.list.names, tolower(distribution$dist))
    dist.invalid <- c(any(is.na(match(dist.attrib, c("dist", "normal", "mean", "sd")))),
                      any(is.na(match(dist.attrib, c("dist", "lognormal", "meanlog", "sdlog")))),
                      any(is.na(match(dist.attrib, c("dist", "uniform", "min", "max")))),
                      any(is.na(match(dist.attrib, c("dist", "exponential", "rate")))),
                      any(is.na(match(dist.attrib, c("dist", "poisson", "lambda")))),
                      any(is.na(match(dist.attrib, c("dist", "binomial", "size", "prob")))),
                      any(is.na(match(dist.attrib, c("dist", "bernoulli", "prob")))))
    if (all(dist.invalid == TRUE)) stop("Unknown input type for `distribution` argument", call. = FALSE)
  } else {
    stop("Unknown input type for `distribution`.", call. = FALSE)
  }

  # asymptotic variances
  var.beta <- function(beta0, beta1, distribution) {

    if (tolower(distribution$dist) == "normal") {

      min <- stats::qnorm(.0000001, mean = distribution$mean, sd = distribution$sd)
      max <- stats::qnorm(.9999999, mean = distribution$mean, sd = distribution$sd)

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dnorm(x, mean = distribution$mean, sd = distribution$sd)
      calcInt <- FALSE

    } else if (tolower(distribution$dist) == "poisson") {

      min <- 0
      max <- stats::qpois(.999999999, lambda = distribution$lambda)

      # define the distribution function and use summation (calcInt == TRUE)
      dist.func <- function(x) stats::dpois(x, lambda = distribution$lambda)
      calcInt <- TRUE

    } else if (tolower(distribution$dist) == "uniform") {

      min <- distribution$min
      max <- distribution$max

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dunif(x, min = min, max = max)
      calcInt <- FALSE

    } else if (tolower(distribution$dist) == "exponential") {

      min <- 0
      max <- stats::qexp(.9999999, rate = distribution$rate)

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dexp(x, rate = distribution$rate)
      calcInt <- FALSE

    } else if (tolower(distribution$dist) %in% c("binomial", "bernoulli")) {

      min <- 0
      max <- ifelse(tolower(distribution$dist) == "bernoulli", 1, distribution$size)

      # define the distribution function and use summation (calcInt == TRUE)
      dist.func <- function(x) stats::dbinom(x, size = max, prob = distribution$prob)
      calcInt <- TRUE

    } else if (tolower(distribution$dist) == "lognormal") {

      min <- stats::qlnorm(.0000001, meanlog = distribution$meanlog, sdlog = distribution$sdlog)
      max <- stats::qlnorm(.9999999, meanlog = distribution$meanlog, sdlog = distribution$sdlog)

      # define the distribution function and use integration (calcInt == FALSE)
      dist.func <- function(x) stats::dlnorm(x, meanlog = distribution$meanlog, sdlog = distribution$sdlog)
      calcInt <- FALSE

    }

    # carry out the actual calculations
    # [1] define the variance function
    var.func <- function(x, e, b0, b1) x ^ e * dist.func(x) * exp(b0 + b1 * x)

    # [2A] use summation to “integrate” integer sequences OR
    if (calcInt) {

      # determine which sequence should be summed up
      calc.seq <- seq(min, max)

      # for mu: e1 [first parm.] = 0 -> x ^ e1 == 1, the log of which is beta0* (beta0s)
      # calculate mu and beta0s -                     | parms. to var.func
      mu  <- sum(vapply(calc.seq, var.func, numeric(1), 0, beta0,  beta1), na.rm = TRUE)
      beta0s <- log(mu)

      # variance under null -                         | parms. to var.func
      i00 <- sum(vapply(calc.seq, var.func, numeric(1), 0, beta0s, 0),     na.rm = TRUE)
      i01 <- sum(vapply(calc.seq, var.func, numeric(1), 1, beta0s, 0),     na.rm = TRUE)
      i11 <- sum(vapply(calc.seq, var.func, numeric(1), 2, beta0s, 0),     na.rm = TRUE)
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative -                  | parms. to var.func
      i00 <- sum(vapply(calc.seq, var.func, numeric(1), 0, beta0,  beta1), na.rm = TRUE)
      i01 <- sum(vapply(calc.seq, var.func, numeric(1), 1, beta0,  beta1), na.rm = TRUE)
      i11 <- sum(vapply(calc.seq, var.func, numeric(1), 2, beta0,  beta1), na.rm = TRUE)
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    # [2B] use integration for real numbers
    } else {

      # for mu: e1 [first parm.] = 0 -> x ^ e1 == 1, the log of which is beta0* (beta0s)
      # calculate mu and beta0s -               | parms. to var.func
      mu  <- stats::integrate(var.func, min, max, 0, beta0,  beta1)$value
      beta0s <- log(mu)

      # variance under null -                   | parms. to var.func
      i00 <- stats::integrate(var.func, min, max, 0, beta0s, 0)$value
      i01 <- stats::integrate(var.func, min, max, 1, beta0s, 0)$value
      i11 <- stats::integrate(var.func, min, max, 2, beta0s, 0)$value
      var.beta0 <- i00 / (i00 * i11 - i01 ^ 2)

      # variance under alternative -            | parms. to var.func
      i00 <- stats::integrate(var.func, min, max, 0, beta0,  beta1)$value
      i01 <- stats::integrate(var.func, min, max, 1, beta0,  beta1)$value
      i11 <- stats::integrate(var.func, min, max, 2, beta0,  beta1)$value
      var.beta1 <- i00 / (i00 * i11 - i01 ^ 2)

    }

    list(var.beta0 = var.beta0, var.beta1 = var.beta1, 
         distribution = tolower(distribution$dist),
         min = min, max = max)

  } # var.beta()

  pwr.demidenko <- function(beta0, beta1, n,
                            r.squared.predictor,
                            alpha, alternative,
                            method, distribution,
                            mean.exposure) {

    # variance correction factor
    if (method == "demidenko(vc)") {
      vcf <- switch(tolower(distribution$dist),
                     `normal` = 1,
                     `poisson` = 1,
                     `uniform` = 1,
                     `exponential` = 1,
                     `binomial` = 1, # 0.85,
                     `bernoulli` = 1, # 0.85,
                     `lognormal` = 0.75)
    } else if (method == "demidenko") {
      vcf <- 0
    } else {
      vcf <- NA
    }

    # non-centrality parameter and standard deviation of the non-centrality parameter under alternative
    # Signorini, D. F. (1991). Sample size for poisson regression. Biometrika, 78, 446-450.
    # Demidenko, E. (2007). Sample size determination for logistic regression revisited. Statistics in Medicine, 26, 3385-3397.
    if(method == "signorini") {
      if(tolower(distribution$dist) == "normal") {
        mean <- distribution$mean
        sd <- distribution$sd
        var.beta0 <- 1 / sd^2
        var.beta1 <- exp(-(beta1 * mean + beta1^2 * sd^2 / 2)) / sd^2 
        ncp <- beta1 / sqrt(var.beta0 / (n * (1 - r.squared.predictor) * mean.exposure))
        sd.ncp <- sqrt(var.beta1 / var.beta0)
      } else if(tolower(distribution$dist) %in% c("binomial", "bernoulli")) {
        ifelse(tolower(distribution$dist) == "bernoulli", size <- 1, size <- distribution$size)
        prob <- distribution$prob
        var.beta0 <- 1 / (prob * (1 - prob))
        var.beta1 <- 1 / (1 - prob) + 1 / (prob * exp(beta1))
        ncp <- beta1 * sqrt(exp(beta0) * n * (1 - r.squared.predictor) * mean.exposure / var.beta0)
        sd.ncp <- sqrt(var.beta1 / var.beta0)
      } else {
        stop("Distribution type is not supported by the Signorini procedure", call. = FALSE)
      }
    } else { # signorini
      var.obj <- var.beta(beta0 = beta0, beta1 = beta1, distribution = distribution)
      var.beta0 <- var.obj$var.beta0
      var.beta1 <- var.obj$var.beta1
      ncp <- beta1 / sqrt(var.beta1 / (n * (1 - r.squared.predictor) * mean.exposure))
      sd.ncp <- sqrt((vcf * var.beta0 + (1 - vcf) * var.beta1) / var.beta1)
    } # demidenko and demidenko(vc)

    pwr.obj <- power.z.test(mean = ncp, sd = sd.ncp, null.mean = 0,
                            alpha = alpha, alternative = alternative,
                            plot = FALSE, verbose = 0)
    power <- pwr.obj$power
    z.alpha <- pwr.obj$z.alpha

    list(power = power, ncp = ncp, sd.ncp = sd.ncp, vcf = vcf, z.alpha = z.alpha)

  } # pwr.demidenko()

  ss.demidenko <- function(beta0, beta1, power,
                           r.squared.predictor,
                           alpha, alternative,
                           method, distribution,
                           mean.exposure) {

    n <- stats::uniroot(function(n) {
      power - pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n,
                            r.squared.predictor = r.squared.predictor,
                            alpha = alpha, alternative = alternative,
                            method = method, distribution = distribution,
                            mean.exposure = mean.exposure)$power
    }, interval = c(2, 1e10))$root

    n

  } # ss.demidenko()
  
  es.demidenko <- function(beta0, beta1.sign, n, power, 
                           r.squared.predictor,
                           alpha, alternative,
                           method, distribution, 
                           mean.exposure) {
    
    # reasonable bounds for logistics
    # beta1.min <- qlogis(0.001) - beta0 
    # beta1.max <- qlogis(0.999) - beta0
    
    # reasonable bounds for poisson - need to be changed
    # overflow <- 700 # 709.7827
    # beta1.min <- (overflow - beta0) / min.x
    # beta1.max <- (overflow - beta0) / max.x
    var.obj <- var.beta(beta0 = beta0, beta1 = beta0, distribution = distribution)
    min.x <- var.obj$min 
    max.x <- var.obj$max 
    
    rate <- c(0.0001, 1e10)
    bound.values <- c((log(min(rate)) - log(mean.exposure) - beta0) / c(min.x, max.x), 
                      (log(max(rate)) - log(mean.exposure) - beta0) / c(min.x, max.x))
    
    beta1.min <- min(bound.values)
    beta1.max <- max(bound.values)
    
    if(beta1.sign %in% c("-", -1, "-1", "negative")) {
      beta1 <- try({
        stats::uniroot(function(beta1) {
          power - pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n,
                                r.squared.predictor = r.squared.predictor,
                                alpha = alpha, alternative = alternative,
                                method = method, distribution = distribution,
                                mean.exposure = mean.exposure)$power
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
                                method = method, distribution = distribution,
                                mean.exposure = mean.exposure)$power
        }, interval = c(0, beta1.max))$root
      })
      
      if(inherits(beta1, "try-error")) 
        stop("Design is not feasible. Try 'beta1.sign = '-'", call. = FALSE)
      
      return(beta1)
      
    } # positive
    
  } # es.demidenko()

  if (requested == "n") {

    n <- ss.demidenko(beta0 = beta0, beta1 = beta1, power = power,
                      r.squared.predictor = r.squared.predictor,
                      alpha = alpha, alternative = alternative,
                      method = method, distribution = distribution,
                      mean.exposure = mean.exposure)

    if (ceiling) n <- ceiling(n)

  }
  
  if (requested == "es") {
    
    beta1 <- es.demidenko(beta0 = beta0, beta1.sign = beta1.sign, 
                          n = n, power = power,
                          r.squared.predictor = r.squared.predictor,
                          alpha = alpha, alternative = alternative,
                          method = method, distribution = distribution,
                          mean.exposure = mean.exposure)
    
    base.rate <- exp(beta0)
    rate.ratio <- exp(beta1)
    
  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.demidenko(beta0 = beta0, beta1 = beta1, n = n, r.squared.predictor = r.squared.predictor,
                           alpha = alpha, alternative = alternative, method = method, distribution = distribution,
                           mean.exposure = mean.exposure)

  power <- pwr.obj$power
  z.alpha <- pwr.obj$z.alpha
  ncp <- pwr.obj$ncp
  sd.ncp <- pwr.obj$sd.ncp
  vcf <- pwr.obj$vcf

  if (verbose > 0) {

    print.obj <- list(requested = requested,
                      test = "Poisson Regression Coefficient (Wald's Z-Test)",
                      method = switch(method,
                                      `demidenko(vc)` = "Demidenko (Variance Corrected)",
                                      `demidenko` = "Demidenko",
                                      `signorini` = "Signorini"),
                      dist = switch(tolower(distribution$dist),
                                    `normal` = "Normal",
                                    `poisson` = "Poisson",
                                    `bernoulli` = "Bernoulli",
                                    `binomial` = "Binomial",
                                    `lognormal` = "Log-normal",
                                    `uniform` = "Uniform",
                                    `exponential` = "Exponential"),
                      alternative = alternative,
                      base.rate = base.rate,
                      rate.ratio = rate.ratio,
                      n = n,
                      mean.alternative = ncp,
                      sd.alternative = sd.ncp,
                      vcf = vcf,
                      mean.null = 0,
                      sd.null = 1,
                      z.alpha = z.alpha,
                      alpha = alpha,
                      power = power)

    .print.pwrss.poisson(print.obj, verbose = verbose, utf = utf)

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           base.rate = base.rate,
                           rate.ratio = rate.ratio,
                           mean = ncp,
                           sd = sd.ncp,
                           vcf = vcf,
                           null.mean = 0,
                           null.sd = 1,
                           z.alpha = z.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "z", "poisson")))
} # power.z.poisson()

#' @export power.z.poisreg
power.z.poisreg <- power.z.poisson


#' @export pwrss.z.poisson
pwrss.z.poisson <- function(exp.beta0 = 1.10, exp.beta1 = 1.16,
                            beta0 = log(exp.beta0), beta1 = log(exp.beta1),
                            mean.exposure = 1, n = NULL, power = NULL, r2.other.x = 0,
                            alpha = 0.05, alternative = c("not equal", "less", "greater"),
                            method = c("demidenko(vc)", "demidenko", "signorini"),
                            distribution = "normal", verbose = TRUE) {

  alternative <- tolower(match.arg(alternative))
  method <- tolower(match.arg(method))
  verbose <- ensure_verbose(verbose)

  if (alternative %in% c("less", "greater")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"

  poisreg.obj <- power.z.poisson(beta0 = beta0, beta1 = beta1, n = n, power = power,
                                 r.squared.predictor = r2.other.x, mean.exposure = mean.exposure,
                                 alpha = alpha, alternative = alternative, method = method,
                                 distribution = distribution, ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.poisson() function. \n")

  invisible(poisreg.obj)

} # pwrss.z.poisson

#' @export pwrss.z.poisreg
pwrss.z.poisreg <- pwrss.z.poisson
