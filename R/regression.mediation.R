####################
# mediation z test #
####################
# 'cp = 0' by default, implying complete mediation (it increases explanatory power of the covariate)
# use `r.squared.mediator` and `r.squared.outcome` to adjust standard error for other predictors in mediation and outcome model

#' Power Analysis for Indirect Effects in a Mediation Model (Z, Joint, and
#' Monte Carlo Tests)
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test
#' indirect effects in a mediation model (Z-Test, Joint Test, and Monte Carlo
#' Interval Test). One can consider explanatory power of the covariates in the
#' mediator and outcome model via specifying R-squared values accordingly.
#' \code{power.z.mediation()} and \code{power.z.med()} are the same functions.
#'
#' Formulas are validated using Monte Carlo simulations.
#'
#' @details
#' * NOTE: The function \code{pwrss.z.mediation()} (or its alias
#'   \code{pwrss.z.med()}) are no longer supported. However, they will remain
#'   available as wrappers for the \code{power.z.mediation} function.
#'
#' @aliases power.z.mediation pwrss.z.mediation power.z.med pwrss.z.med
#'
#'
#' @param beta.a              regression coefficient for X -> M path. One can
#'                            use standardized regression coefficient, but
#'                            should keep \code{sd.predictor = 1} and
#'                            \code{sd.mediator = 1} or leave them out as they
#'                            are default specifications.
#' @param beta.b              regression coefficient for M -> Y path. One can
#'                            use standardized regression coefficient, but
#'                            should keep \code{sd.mediator = 1} and
#'                            \code{sd.outcome = 1} or leave them out as they
#'                            are default specifications.
#' @param ab.ratio            'beta.a' / 'beta.b' ratio (can be negative) when minimum 
#'                            detectable effect is of interest (either beta.a or beta.b, or both).
#' @param sign                sign of the indirect effect (beta.a * beta.b product), when minimum 
#'                            detectable effect is of interest (either beta.a or beta.b, or both)).
#' @param beta.cp             regression coefficient for X -> Y path (the
#'                            direct path). One can use standardized regression
#'                            coefficient, but should keep
#'                            \code{sd.predictor = 1} and \code{sd.outcome = 1}
#'                            or leave them out as they are default
#'                            specifications.
#' @param sd.predictor        standard deviation of the predictor (X). For a
#'                            binary predictor,
#'                            \code{sd.predictor = sqrt(p * (1 - p))} where
#'                            \code{p} is the proportion of subjects in one of
#'                            the groups.
#' @param sd.mediator         standard deviation of the mediator (M).
#' @param sd.outcome          standard deviation of the outcome (Y).
#' @param r.squared.mediator  R-squared value for the mediator model (M ~ X).
#'                            The default is \code{r.squared.mediator =
#'                            beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2}
#'                            assuming that X is the only predictor. Thus, an
#'                            \code{r.squared.mediator} below this value will
#'                            throw a warning. To consider other covariates in
#'                            the mediator model provide a value greater than
#'                            the default.
#' @param r.squared.outcome   R-squared value for the outcome model
#'                            (Y ~ M + X). The default is
#'                            \code{r.squared.outcome = (beta.b ^ 2 *
#'                            sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2)
#'                            / sd.outcome ^ 2} assuming that M and X are the
#'                            only predictors. Thus, an
#'                            \code{r.squared.outcome} below this value will
#'                            throw a warning. To consider other covariates in
#'                            the outcome model provide a value greater than
#'                            the default.
#' @param n                   integer; sample size.
#' @param power               statistical power, defined as the probability of
#'                            correctly rejecting a false null hypothesis,
#'                            denoted as \eqn{1 - \beta}.
#' @param alpha               type 1 error rate, defined as the probability of
#'                            incorrectly rejecting a true null hypothesis,
#'                            denoted as \eqn{\alpha}.
#' @param alternative         character; the direction or type of the
#'                            hypothesis test: "two.sided" or "one.sided".
#' @param method              character; "sobel", "aroian", "goodman", "joint"
#'                            or "monte.carlo". "joint" and "monte.carlo"
#'                            methods cannot be used for sample size
#'                            calculation.
#' @param n.simulation        integer; number of replications (applies when
#'                            method = "monte.carlo").
#' @param n.draws             integer; number of draws from the distribution of
#'                            the path coefficients for each replication
#'                            (applies when method = "monte.carlo").
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
#'   \item{test}{type of the statistical test ("Z-Test", "Joint Test", or
#'               "Monte Carlo Interval Test").}
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{z.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{n}{sample size.}
#'
#' @references
#'   Aroian, L. A. (1947). The probability function of the product of
#'   two normally distributed variables. *Annals of Mathematical Statistics,
#'   18*(2), 265-271.
#'
#'   Goodman, L. A. (1960). On the exact variance of products. *Journal of the
#'   American Statistical Association, 55*(292), 708-713.
#'
#'   MacKinnon, D. P., & Dwyer, J. H. (1993). Estimating mediated effects in
#'   prevention studies. *Evaluation Review, 17*(2), 144-158.
#'
#'   MacKinnon, D. P., Warsi, G., & Dwyer, J. H. (1995). A simulation study of
#'   mediated effect measures. *Multivariate Behavioral Research, 30*(1),
#'   41-62.
#'
#'   Preacher, K. J., & Hayes, A. F. (2004). SPSS and SAS procedures for
#'   estimating indirect effects in simple mediation models. *Behavior Research
#'   Methods, Instruments, & Computers, 36*, 717-731.
#'
#'   Preacher, K. J., & Hayes, A. F. (2008). Asymptotic and resampling strategies
#'   for assessing and comparing indirect effects in multiple mediator models.
#'   *Behavior Research Methods, 40*, 879-891.
#'
#'   Sobel, M. E. (1982). Asymptotic intervals for indirect effects in structural
#'   equations models. In S. Leinhart (Ed.), *Sociological methodology 1982* (pp.
#'   290-312). Jossey-Bass.
#'
#' @examples
#' # with standardized coefficients
#'
#' ## statistical power
#' power.z.mediation(beta.a = 0.25,
#'             beta.b = 0.25,
#'             beta.cp = 0.10,
#'             n = 200)
#'
#' ## minimum required sample size
#' power.z.mediation(beta.a = 0.25,
#'             beta.b = 0.25,
#'             beta.cp = 0.10,
#'             power = 0.80)
#'
#' ## adjust for covariates in the outcome model
#' power.z.mediation(beta.a = 0.25,
#'             beta.b = 0.25,
#'             beta.cp = 0.10,
#'             r.squared.outcome = 0.50,
#'             power = 0.80)
#'
#' # with binary predictor X such as treatment/control variable
#' # in this case standardized coefficients for path a and cp would be Cohen's d values
#'
#' ## statistical power
#' p <- 0.50 # proportion of subjects in one group
#' power.z.mediation(beta.a = 0.40,
#'             beta.b = 0.25,
#'             beta.cp = 0.10,
#'             sd.predictor = sqrt(p*(1-p)),
#'             n = 200)
#'
#' ## minimum required sample size
#' power.z.mediation(beta.a = 0.40,
#'             beta.b = 0.25,
#'             beta.cp = 0.10,
#'             sd.predictor = sqrt(p*(1-p)),
#'             power = 0.80)
#'
#' ## adjust for covariates in the outcome model
#' power.z.mediation(beta.a = 0.40,
#'             beta.b = 0.25, beta.cp = 0.10,
#'             r.squared.outcome = 0.50,
#'             sd.predictor = sqrt(p*(1-p)),
#'             power = 0.80)
#'
#' @export power.z.mediation
power.z.mediation  <- function(beta.a = NULL, beta.b = NULL, ab.ratio = 1, sign = "+", beta.cp = 0,
                               sd.predictor = 1, sd.mediator = 1, sd.outcome = 1,
                               r.squared.mediator = NULL,
                               r.squared.outcome = NULL,
                               n = NULL, power = NULL, alpha = 0.05,
                               alternative = c("two.sided", "one.sided"),
                               method = c("sobel", "aroian", "goodman", "joint", "monte.carlo"),
                               n.simulation = 1000, n.draws = 1000,
                               ceiling = TRUE, verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  method <- tolower(match.arg(method))
  func.parms <- clean.parms(as.list(environment()))

  if (!is.null(beta.a)) check.numeric(beta.a)
  if (!is.null(beta.b)) check.numeric(beta.b)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  
  if (!is.null(r.squared.mediator)) {
    check.proportion(r.squared.mediator)
    if (!is.null(beta.a))
      if(r.squared.mediator < beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2)
        warning("Specified `r.squared.mediator` is smaller than the base `r.squared.mediator`.", call. = FALSE)
  }
  
  if (!is.null(r.squared.outcome)) {
    check.proportion(r.squared.outcome)
    if (r.squared.outcome == 0 && "beta.cp" %in% names(match.call()))
      warning("Ignoring any specification to `beta.cp`.", call. = FALSE)
    if (!is.null(beta.b))
      if (r.squared.outcome < (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2)
        warning("Specified `r.squared.outcome` is smaller than the base `r.squared.outcome`.", call. = FALSE)
  }
    
  check.numeric(beta.cp)
  beta.cp <- ifelse(is.null(beta.cp), 0, beta.cp)
  check.proportion(alpha)
  check.sample.size(n.simulation, n.draws)
  check.nonnegative(sd.predictor, sd.mediator, sd.outcome)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)

  # requested <- check.n_power(n, power)
  if(is.null(n)) requested <- "n"
  if(is.null(power)) requested <- "power"
  if(is.null(beta.a) || is.null(beta.b)) requested <- "es"
  
  se.a <- function(sd.mediator, sd.predictor, r.squared.mediator, n) {
    var.beta.a <- (1 / n) * (sd.mediator ^ 2) * (1 - r.squared.mediator) / (sd.predictor ^ 2)
    sqrt(var.beta.a)
  }

  se.b <- function(sd.outcome, sd.mediator, r.squared.outcome, r.squared.mediator, n) {
    var.beta.b <- (1 / n) * (sd.outcome ^ 2) * (1 - r.squared.outcome) / ((sd.mediator ^ 2) * (1 - r.squared.mediator))
    sqrt(var.beta.b)
  }

  pwr.med <- function(beta.a, beta.b,
                      sd.predictor, sd.mediator, sd.outcome,
                      r.squared.mediator, r.squared.outcome,
                      n, method = c("sobel", "aroian", "goodman",
                                    "joint", "monte.carlo")) {

    method <- tolower(match.arg(method))

    se.beta.a <- se.a(sd.mediator = sd.mediator, sd.predictor = sd.predictor,
                      r.squared.mediator = r.squared.mediator, n = n)
    se.beta.b <- se.b(sd.outcome = sd.outcome, sd.mediator = sd.mediator,
                      r.squared.outcome = r.squared.outcome,
                      r.squared.mediator = r.squared.mediator, n = n)

    if (method == "sobel") {
      sobel.se <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2)
      lambda <- (beta.a * beta.b) / sobel.se
      pwr.obj <- power.z.test(mean = lambda, alpha = alpha, alternative = alternative,
                              plot = FALSE, verbose = 0)
    }

    if (method == "aroian") {
      aroian.se <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 + se.beta.a ^ 2 * se.beta.b ^ 2)
      lambda <- (beta.a * beta.b) / aroian.se
      pwr.obj <- power.z.test(mean = lambda, alpha = alpha, alternative = alternative,
                              plot = FALSE, verbose = 0)
    }

    if (method == "goodman") {
      goodman.var <- beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 - se.beta.a ^ 2 * se.beta.b ^ 2
      if (goodman.var <= 0) stop("Design is not feasible for Goodman's Z-Test", call. = FALSE)
      goodman.se <- sqrt(goodman.var)
      lambda <- (beta.a * beta.b) / goodman.se
      pwr.obj <- power.z.test(mean = lambda, alpha = alpha, alternative = alternative,
                              plot = FALSE, verbose = 0)
    }

    if (method == "joint") {
      power.beta.a <- power.z.test(mean = beta.a / se.beta.a, alpha = alpha, alternative = alternative,
                                   plot = FALSE, verbose = 0)$power
      power.beta.b <- power.z.test(mean = beta.b / se.beta.b, alpha = alpha, alternative = alternative,
                                   plot = FALSE, verbose = 0)$power
      power <- power.beta.a * power.beta.b
      pwr.obj <- list(alternative = alternative, mean = NA, sd = NA, null.mean = NA,
                      alpha = alpha, z.alpha = NA, power = power)
    }

    if (method == "monte.carlo") {
      beta.a <- abs(beta.a)
      beta.b <- abs(beta.b)
      reject <- numeric(0)
      for (i in 1:n.simulation) {
        beta.a.star <- stats::rnorm(1, beta.a, se.beta.a)
        beta.b.star <- stats::rnorm(1, beta.b, se.beta.b)
        reject <- c(reject, stats::quantile(stats::rnorm(n.draws, beta.a.star, se.beta.a) * stats::rnorm(n.draws, beta.b.star, se.beta.b),
                                     probs = ifelse(alternative == "two.sided", alpha / 2, alpha), na.rm = TRUE) > 0)
      }
      power <- mean(reject)
      pwr.obj <- list(alternative = alternative, mean = NA, sd = NA, null.mean = NA,
                      alpha = alpha, z.alpha = NA, power = power)
    }

    pwr.obj

  } # pwr.med()

  ss.med <- function(beta.a, beta.b,
                     sd.predictor, sd.mediator, sd.outcome,
                     r.squared.mediator, r.squared.outcome,
                     power, method = c("sobel", "aroian", "goodman")) {

    method <- tolower(match.arg(method))

    if (method == "sobel") {
      n <- stats::uniroot(function(n) {
        se.beta.a <- se.a(sd.mediator = sd.mediator, sd.predictor = sd.predictor,
                          r.squared.mediator = r.squared.mediator, n = n)
        se.beta.b <- se.b(sd.outcome = sd.outcome, sd.mediator = sd.mediator,
                          r.squared.outcome = r.squared.outcome,
                          r.squared.mediator = r.squared.mediator, n = n)
        sobel.se <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2)
        lambda.sobel <- (beta.a * beta.b) / sobel.se
        power - power.z.test(mean = lambda.sobel, alpha = alpha, alternative = alternative,
                             plot = FALSE, verbose = 0)$power
      }, interval = c(10, 1e10))$root
    }

    if (method == "aroian") {
      n <- stats::uniroot(function(n) {
        se.beta.a <- se.a(sd.mediator = sd.mediator, sd.predictor = sd.predictor,
                          r.squared.mediator = r.squared.mediator, n = n)
        se.beta.b <- se.b(sd.outcome = sd.outcome, sd.mediator = sd.mediator,
                          r.squared.outcome = r.squared.outcome,
                          r.squared.mediator = r.squared.mediator, n = n)
        aroian.se <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 + se.beta.a ^ 2 * se.beta.b ^ 2)
        lambda.aroian <- (beta.a * beta.b) / aroian.se
        power - power.z.test(mean = lambda.aroian, alpha = alpha, alternative = alternative,
                             plot = FALSE, verbose = 0)$power
      }, interval = c(10, 1e10))$root
    }

    if (method == "goodman") {
      n <- stats::uniroot(function(n) {
        se.beta.a <- se.a(sd.mediator = sd.mediator, sd.predictor = sd.predictor,
                          r.squared.mediator = r.squared.mediator, n = n)
        se.beta.b <- se.b(sd.outcome = sd.outcome, sd.mediator = sd.mediator,
                          r.squared.outcome = r.squared.outcome,
                          r.squared.mediator = r.squared.mediator, n = n)
        if (beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 < se.beta.a ^ 2 * se.beta.b ^ 2) {
          goodman.se <- 1
        } else {
          goodman.se <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 - se.beta.a ^ 2 * se.beta.b ^ 2)
        }
        lambda.goodman <- (beta.a * beta.b) / goodman.se
        power - power.z.test(mean = lambda.goodman, alpha = alpha, alternative = alternative,
                             plot = FALSE, verbose = 0)$power
      }, interval = c(10, 1e10))$root
    }

    n

  } # ss.med()
  
  es.med <- function(beta.a, beta.b, ab.ratio, beta.cp, 
                     sd.predictor, sd.mediator, sd.outcome,
                     r.squared.mediator, r.squared.outcome,
                     n, power, method) {
    
    # lim.a <- sqrt(1 - r.squared.mediator) * (sd.mediator / sd.predictor)
    # lim.b <- sqrt(1 - r.squared.outcome) * (sd.outcome / sd.mediator)
    lim.a <- abs(ab.ratio) * (sd.mediator / sd.predictor)
    lim.b <- abs(1 /  ab.ratio) * (sd.outcome / sd.mediator)
    
    if(is.null(beta.a) & !is.null(beta.b)) {
      
      if(sign %in% c("-", -1, "-1", "negative")) {
        ifelse(sign(beta.b) == -1, bounds <- c(0, lim.a), bounds <- c(-lim.a, 0))
        beta.a <- try({
          stats::optimize(f = function(beta.a) {
            if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
            if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2
            (power - pwr.med(beta.a = beta.a, beta.b = beta.b,
                             sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                             r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                             n = n, method = method)$power)^2
          }, interval = bounds, maximum = FALSE)$minimum
        })
        
        if(inherits(beta.a, "try-error")) 
          stop("Design is not feasible. Try sign = '+'", call. = FALSE)
        
      } # negative
      
      if(sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) {
        ifelse(sign(beta.b) == 1, bounds <- c(0, lim.a), bounds <- c(-lim.a, 0))
        
        beta.a <-  try({
          stats::optimize(f = function(beta.a) {
            if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
            if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2
            (power - pwr.med(beta.a = beta.a, beta.b = beta.b,
                             sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                             r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                             n = n, method = method)$power)^2
          }, interval = bounds, maximum = FALSE)$minimum
        })
        
        
        if(inherits(beta.a, "try-error")) 
          stop("Design is not feasible. Try sign = '-'", call. = FALSE)
        
      } # positive
      
    } # beta.a
    
    if(is.null(beta.b) & !is.null(beta.a)) {
      
      if(sign %in% c("-", -1, "-1", "negative")) {
        ifelse(sign(beta.a) == -1, bounds <- c(0, lim.b), bounds <- c(-lim.b, 0))
        beta.b <- try({
          stats::optimize(f = function(beta.b) {
            if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
            if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2
            (power - pwr.med(beta.a = beta.a, beta.b = beta.b,
                             sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                             r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                             n = n, method = method)$power)^2
          }, interval = bounds, maximum = FALSE)$minimum
        })
        
        if(inherits(beta.b, "try-error")) 
          stop("Design is not feasible. Try sign = '+'", call. = FALSE)
        
      } # negative
      
      if(sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) {
        ifelse(sign(beta.a) == 1, bounds <- c(0, lim.b), bounds <- c(-lim.b, 0))
        beta.b <-  try({
          stats::optimize(f = function(beta.b) {
            if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
            if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2
            (power - pwr.med(beta.a = beta.a, beta.b = beta.b,
                             sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                             r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                             n = n, method = method)$power)^2
          }, interval = bounds, maximum = FALSE)$minimum
        })
        
        if(inherits(beta.b, "try-error")) 
          stop("Design is not feasible. Try sign = '-'", call. = FALSE)
        
      } # positive
      
    } # beta.b
    
    if(is.null(beta.b) & is.null(beta.b)) {
      
      if(sign %in% c("-", -1, "-1", "negative")) {
        if(sign(ab.ratio) == 1) {
          warning("'sign' is negative but 'ab.ratio' is positive. Ignoring the sign of 'ab.ratio' but not its value.", call. = FALSE)
          ab.ratio <- -ab.ratio
        }
        ifelse(sign(ab.ratio) == -1, bounds <- c(0, lim.b), bounds <- c(-lim.b, 0))
        beta.b <- try({
          stats::optimize(f = function(beta.b) {
            beta.a <- beta.b * ab.ratio
            if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
            if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2
            (power - pwr.med(beta.a = beta.a, beta.b = beta.b,
                             sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                             r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                             n = n, method = method)$power)^2
          }, interval = bounds, maximum = FALSE)$minimum
        })
        
        if(inherits(beta.b, "try-error")) 
          stop("Design is not feasible. Try sign = '+'", call. = FALSE)
        
        beta.a <- beta.b * ab.ratio
       
      } # negative
      
      if(sign %in% c("+", 1, "1", "+1", "positive", "pozitive")) {
        if(sign(ab.ratio) == -1) {
          warning("'sign' is positive but 'ab.ratio' is negative. Ignoring the sign of 'ab.ratio' but not its value.", call. = FALSE)
          ab.ratio <- -ab.ratio
        }
        ifelse(sign(ab.ratio) == 1, bounds <- c(0, lim.b), bounds <- c(-lim.b, 0))
        beta.b <-  try({
          stats::optimize(f = function(beta.b) {
            beta.a <- beta.b * ab.ratio
            if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
            if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2
            (power - pwr.med(beta.a = beta.a, beta.b = beta.b,
                             sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                             r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                             n = n, method = method)$power)^2
          }, interval = bounds, maximum = FALSE)$minimum
        })
        
        beta.a <- beta.b * ab.ratio
        
        if(inherits(beta.b, "try-error")) 
          stop("Design is not feasible. Try sign = '-'", call. = FALSE)
        
      } # positive
      
    } # beta.a and beta.b
    
    list(beta.a = beta.a, beta.b = beta.b)
    
  } # es.med() 

  if (requested == "n") {

    if (method %in% c("joint", "monte.carlo"))
      stop("Sample size calculation not supported by this method.", call. = FALSE)

    n <- ss.med(beta.a = beta.a, beta.b = beta.b,
                sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                power = power, method = method)

  } # sample size
  
  if (ceiling) n <- ceiling(n)
  
  if (requested == "es") {
    
    if (method %in% c("joint", "monte.carlo"))
      stop("Minimum detectable effect calculation not supported by this method.", call. = FALSE)
    
    es.obj <- es.med(beta.a = beta.a, beta.b = beta.b, ab.ratio = ab.ratio, beta.cp = beta.cp, 
                     sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                     r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                     n = n, power = power, method = method)
    
    beta.a <- es.obj$beta.a
    beta.b <- es.obj$beta.b
    
  } # effect size
  
  
  if (is.null(r.squared.mediator)) r.squared.mediator <- beta.a ^ 2 * sd.predictor ^ 2 / sd.mediator ^ 2
  if (is.null(r.squared.outcome)) r.squared.outcome <- (beta.b ^ 2 * sd.mediator ^ 2 + beta.cp ^ 2 * sd.predictor ^ 2) / sd.outcome ^ 2

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.med(beta.a = beta.a, beta.b = beta.b,
                     sd.predictor = sd.predictor, sd.mediator = sd.mediator, sd.outcome = sd.outcome,
                     r.squared.mediator = r.squared.mediator, r.squared.outcome = r.squared.outcome,
                     n = n, method = method)

  power <- pwr.obj$power
  mean.alternative <- pwr.obj$mean
  sd.alternative <- 1
  mean.null <- pwr.obj$null.mean
  sd.null <- 1
  z.alpha <- pwr.obj$z.alpha

  beta.indirect <- beta.a * beta.b
  std.beta.a <- beta.a * sd.predictor / sd.mediator
  std.beta.b <- beta.b * sd.mediator / sd.outcome
  std.beta.cp <- beta.cp * sd.predictor / sd.outcome
  std.beta.indirect <-  std.beta.a * std.beta.b

  if (verbose > 0) {

    print.obj <- list(test = "Indirect Effect in a Mediation Model",
                      method = method,
                      requested = requested,
                      std.beta.a = std.beta.a,
                      std.beta.b = std.beta.b,
                      std.beta.cp = std.beta.cp,
                      std.beta.indirect = std.beta.indirect,
                      mean.alternative = mean.alternative,
                      sd.alternative = sd.alternative,
                      mean.null = mean.null,
                      sd.null = sd.null,
                      alpha = alpha,
                      alternative = alternative,
                      z.alpha = z.alpha,
                      n = n,
                      power = power)

    .print.pwrss.med(print.obj, verbose = verbose, utf = utf)

  } # verbose

  invisible(structure(list(parms = func.parms,
                           test = switch(method, `joint` = "joint", `monte.carlo` = "monte.carlo", "z"),
                           beta.a = beta.a,
                           beta.b = beta.b,
                           beta.indirect = beta.indirect,
                           beta.cp = beta.cp,
                           r.squared.mediator = r.squared.mediator,
                           r.squared.outcome = r.squared.outcome,
                           std.beta.a = std.beta.a,
                           std.beta.b = std.beta.b,
                           std.beta.cp = std.beta.cp,
                           std.beta.indirect = std.beta.indirect,
                           mean = mean.alternative,
                           sd = sd.alternative,
                           null.mean = mean.null,
                           null.sd = sd.null,
                           z.alpha = z.alpha,
                           power = power,
                           n = n),
                      class = c("pwrss", "z", "mediation")))

} # end of power.z.mediation()

#' @export power.z.med
power.z.med <- power.z.mediation


#' @export pwrss.z.mediation
pwrss.z.mediation  <- function(a, b, cp = 0,
                               sdx = 1, sdm = 1, sdy = 1,
                               r2m.x = a ^ 2 * sdx ^ 2 / sdm ^ 2,
                               r2y.mx = (b ^ 2 * sdm ^ 2 + cp ^ 2 * sdx ^ 2) / sdy ^ 2,
                               n = NULL, power = NULL,
                               alpha = 0.05, alternative = c("not equal", "less", "greater"),
                               mc = TRUE, nsims = 1000, ndraws = 1000, verbose = TRUE) {

  alternative <- tolower(match.arg(alternative))
  if (alternative %in% c("less", "greater")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"
  func.parms <- clean.parms(as.list(environment()))
  verbose <- ensure_verbose(verbose)

  if (is.null(power)) {

    sobel.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                   sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                   r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                   n = n, power = power, alpha = alpha,
                                   alternative = alternative,
                                   method = "sobel",
                                   n.simulation = nsims,
                                   n.draws = ndraws,
                                   ceiling = TRUE,
                                   verbose = 0)

    aroian.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                    sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                    r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                    n = n, power = power, alpha = alpha,
                                    alternative = alternative,
                                    method = "aroian",
                                    n.simulation = nsims,
                                    n.draws = ndraws,
                                    ceiling = TRUE,
                                    verbose = 0)

    goodman.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                     sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                     r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                     n = n, power = power, alpha = alpha,
                                     alternative = alternative,
                                     method = "goodman",
                                     n.simulation = nsims,
                                     n.draws = ndraws,
                                     ceiling = TRUE,
                                     verbose = 0)

    joint.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                   sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                   r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                   n = n, power = power, alpha = alpha,
                                   alternative = alternative,
                                   method = "joint",
                                   n.simulation = nsims,
                                   n.draws = ndraws,
                                   ceiling = TRUE,
                                   verbose = 0)

    monte.carlo.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                         sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                         r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                         n = n, power = power, alpha = alpha,
                                         alternative = alternative,
                                         method = "monte.carlo",
                                         n.simulation = nsims,
                                         n.draws = ndraws,
                                         ceiling = TRUE,
                                         verbose = 0)

    power.out <- data.frame(rbind(
      c(sobel.obj$mean, sobel.obj$n, sobel.obj$power),
      c(aroian.obj$mean, aroian.obj$n, aroian.obj$power),
      c(goodman.obj$mean, goodman.obj$n, goodman.obj$power),
      c(joint.obj$mean, joint.obj$n, joint.obj$power),
      c(monte.carlo.obj$mean, monte.carlo.obj$n, monte.carlo.obj$power)
    ))

    colnames(power.out) <- c("non-centrality", "n", "power")
    power.out$method <- c("Sobel", "Aroian", "Goodman", "Joint", "Monte Carlo")

  } else if (is.null(n)) {

    sobel.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                   sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                   r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                   n = n, power = power, alpha = alpha,
                                   alternative = alternative,
                                   method = "sobel",
                                   n.simulation = nsims,
                                   n.draws = ndraws,
                                   ceiling = TRUE,
                                   verbose = 0)

    aroian.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                    sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                    r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                    n = n, power = power, alpha = alpha,
                                    alternative = alternative,
                                    method = "aroian",
                                    n.simulation = nsims,
                                    n.draws = ndraws,
                                    ceiling = TRUE,
                                    verbose = 0)

    goodman.obj <- power.z.mediation(beta.a = a, beta.b = b, beta.cp = cp,
                                     sd.predictor = sdx, sd.mediator = sdm, sd.outcome = sdy,
                                     r.squared.mediator = r2m.x, r.squared.outcome = r2y.mx,
                                     n = n, power = power, alpha = alpha,
                                     alternative = alternative,
                                     method = "goodman",
                                     n.simulation = nsims,
                                     n.draws = ndraws,
                                     ceiling = TRUE,
                                     verbose = 0)

    power.out <- data.frame(rbind(
      c(sobel.obj$mean, sobel.obj$n, sobel.obj$power),
      c(aroian.obj$mean, aroian.obj$n, aroian.obj$power),
      c(goodman.obj$mean, goodman.obj$n, goodman.obj$power),
      c(NA, NA, NA),
      c(NA, NA, NA)
    ))

    colnames(power.out) <- c("non-centrality", "n", "power")
    power.out$method <- c("Sobel", "Aroian", "Goodman", "Joint", "Monte Carlo")

  }


  if (is.null(power)) {
    ncp.vec <- c(sobel = power.out$mean[1], aroian = power.out$mean[2], goodman = power.out$mean[3], joint = power.out$mean[4],
                 mc = power.out$mean[5])
    n.vec <- c(sobel = power.out$n[1], aroian = power.out$n[2], goodman = power.out$n[3], joint = power.out$n[4], mc = power.out$n[5])
    power.vec <- c(sobel = power.out$power[1], aroian = power.out$power[2], goodman = power.out$power[3], joint = power.out$power[4],
                   mc = power.out$power[5])
  } else if (is.null(n)) {
    ncp.vec <- c(sobel = power.out$mean[1], aroian = power.out$mean[2], goodman = power.out$mean[3], joint = power.out$mean[4],
                 mc = power.out$mean[5])
    n.vec <- c(sobel = power.out$n[1], aroian = power.out$n[2], goodman = power.out$n[3], joint = power.out$n[4], mc = power.out$n[5])
    power.vec <- c(sobel = power.out$power[1], aroian = power.out$power[2], goodman = power.out$power[3], joint = power.out$power[4],
                   mc = power.out$power[5])
  }

  if (verbose > 0) {
    cat(" Indirect Effect in a Mediation Model",
        "\n ====================================\n",
        sep = "")

    ifelse(is.null(power),
           print(power.out[,    c("method", "non-centrality", "n", "power")], row.names = FALSE),
           print(power.out[1:3, c("method", "non-centrality", "n", "power")], row.names = FALSE))

    cat(" ------------------------------------\n",
        " Type 1 error rate: ", round(alpha, 3), "\n\n",
        sep = "")

    # cat2("beta.indirect := beta.a * beta.b \n", "green")

  } # verbose

# cat("This function will be removed in the future. \n Please use power.z.mediation() function. \n")

invisible(structure(list(parms = func.parms,
                         test = c("z", "joint", "monte.carlo"),
                         ncp = ncp.vec,
                         power = power.vec,
                         n = n.vec),
                    class = c("pwrss", "z", "med", "defunct")))

} # pwrss.z.mediation()

#' @export pwrss.z.med
pwrss.z.med  <- pwrss.z.mediation
