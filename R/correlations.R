#######################################################################
# Steiger's z-test for dependent correlations (Steiger, 1980, p. 247) #
#######################################################################

#' Power Analysis for Dependent Correlations (Steiger's Z-Test)
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test
#' difference between paired correlations (Pearson) using Fisher's
#' Z-transformation.
#'
#' Validated via PASS and G*Power.
#'
#' @aliases power.z.twocors.steiger power.z.steiger
#'
#'
#' @param rho12        correlation between variable V1 and V2 (one common index
#'                     and no common index). Check examples below.
#' @param rho13        correlation between variable V1 and V3 (one common index
#'                     and no common index). Check examples below.
#' @param rho23        correlation between variable V2 and V3 (one common index
#'                     and no common index). Check examples below.
#' @param rho14        correlation between variable V1 and V4 (no common index
#'                     only). Check examples below.
#' @param rho24        correlation between variable V2 and V4 (no common index
#'                     only). Check examples below.
#' @param rho34        correlation between variable V3 and V4 (no common index
#'                     only). Check examples below.
#' @param n            integer; sample size.
#' @param power        statistical power, defined as the probability of
#'                     correctly rejecting a false null hypothesis, denoted as
#'                     \eqn{1 - \beta}.
#' @param alpha        type 1 error rate, defined as the probability of
#'                     incorrectly rejecting a true null hypothesis, denoted as
#'                     \eqn{\alpha}.
#' @param alternative  character; the direction or type of the hypothesis test:
#'                     "two.sided" or "one.sided".
#' @param pooled       logical; whether standard error should be pooled.
#'                     \code{TRUE} by default.
#' @param common.index logical; whether calculations pertain to one common
#'                     index. \code{TRUE} means calculations involve
#'                     correlations with a common index (where both
#'                     correlations share one variable). \code{FALSE} (default)
#'                     means calculations pertain to correlations with no
#'                     common index (where all relevant correlations must be
#'                     explicitly specified). Check examples below.
#' @param ceiling      logical; if \code{TRUE} rounds up sample size.
#' @param verbose      \code{1} by default (returns test, hypotheses, and
#'                     results), if \code{2} a more detailed output is given
#'                     (plus key parameters and definitions), if \code{0} no
#'                     output is printed on the console.
#' @param utf          logical; whether the output should show Unicode
#'                     characters (if encoding allows for it). \code{FALSE} by
#'                     default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (Z-Test)}
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{z.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}.}
#'   \item{n}{sample size for the first and second groups, in the form of
#'            c(n1, n2).}
#'
#' @references
#'   Steiger, J. H. (1980). Tests for comparing elements of a correlation
#'   matrix. *Psychological Bulletin, 87*(2), 245-251.
#'   https://doi.org/10.1037/0033-2909.87.2.245
#'
#' @examples
#' # example data for one common index
#' # compare cor(V1, V2) to cor(V1, V3)
#'
#' # subject    V1       V2      V3
#' # <int>    <dbl>    <dbl>    <dbl>
#' #   1       1.2      2.3      0.8
#' #   2      -0.0      1.1      0.7
#' #   3       1.9     -0.4     -2.3
#' #   4       0.7      1.3      0.4
#' #   5       2.1     -0.1      0.8
#' #   ...     ...      ...      ...
#' #   1000   -0.5      2.7     -1.7
#'
#' # V1: socio-economic status (common)
#' # V2: pretest
#' # V3: post-test
#'
#' power.z.twocors.steiger(rho12 = 0.35, rho13 = 0.45, rho23 = 0.05,
#'                         n = 1000, power = NULL, alpha = 0.05,
#'                         alternative = "two.sided",
#'                         common.index = TRUE)
#'
#'
#' # example data for no common index
#' # compare cor(V1, V2) to cor(V3, V4)
#'
#' # subject    V1       V2       V3       V4
#' # <int>    <dbl>    <dbl>    <dbl>    <dbl>
#' #   1       1.2      2.3      0.8      1.2
#' #   2      -0.0      1.1      0.7      0.9
#' #   3       1.9     -0.4     -2.3     -0.1
#' #   4       0.7      1.3      0.4     -0.3
#' #   5       2.1     -0.1      0.8      2.7
#' #   ...     ...      ...      ...      ...
#' #   1000   -0.5      2.7     -1.7      0.8
#'
#' # V1: pretest reading
#' # V2: pretest math
#' # V3: post-test reading
#' # V4: post-test math
#'
#' power.z.twocors.steiger(rho12 = 0.45, rho13 = 0.45, rho23 = 0.50,
#'                         rho14 = 0.50, rho24 = 0.80, rho34 = 0.55,
#'                         n = 1000, power = NULL, alpha = 0.05,
#'                         alternative = "two.sided",
#'                         common.index = FALSE)
#'
#' @export power.z.twocors.steiger
power.z.twocors.steiger <- function(rho12, rho13, rho23,
                            rho14 = NULL, rho24 = NULL, rho34 = NULL,
                            n = NULL, power = NULL, alpha = 0.05,
                            alternative = c("two.sided", "one.sided"),
                            pooled = TRUE, common.index = FALSE,
                            ceiling = TRUE, verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  func.parms <- clean.parms(as.list(environment()))

  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(pooled, common.index, ceiling, utf)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  pwr.steiger <- function(rho1, rho2, cov.null, cov.alt, n, alpha, alternative) {

    z1 <- cor.to.z(rho1, FALSE)$z
    z2 <- cor.to.z(rho2, FALSE)$z

    sigma.null <- sqrt((2 - 2 * cov.null) / (n - 3))
    sigma.alt <- sqrt((2 - 2 * cov.alt) / (n - 3))

    lambda <- (z1 - z2) /  sigma.null
    sigma.lambda <- sigma.alt / sigma.null

    if (alternative == "two.sided") lambda <- abs(lambda)

    pwr.obj <- power.z.test(mean = lambda, sd = sigma.lambda,
                            null.mean = 0, null.sd = 1,
                            alpha = alpha,
                            alternative = alternative,
                            plot = FALSE, verbose = 0)

    pwr.obj

  } # pwr.steiger()

  ss.steiger <- function(rho1, rho2,
                         cov.null, cov.alt,
                         power, alpha, alternative) {

    n <- try(silent = TRUE,
             suppressWarnings({
               stats::uniroot(function(n) {
                 power - pwr.steiger(rho1 = rho1, rho2 = rho2,
                                     cov.null = cov.null, cov.alt = cov.alt,
                                     n = n, alpha = alpha,
                                     alternative = alternative)$power
               }, interval = c(5, 1e+09))$root
             }) # supressWarnings
    ) # try

    if (inherits(n, "try-error") || n == 1e10) stop("Design is not feasible.", call. = FALSE)

    n

  } # ss.steiger()

  if (common.index) {

    if (any(check.not_null(rho14, rho24, rho34)))
      warning("Ignoring `rho14` `rho24`, or `rho34` because `common.index` is TRUE.", call. = FALSE)

    check.correlation(rho12, rho13, rho23)

    if (alternative == "two.sided" && rho12 == rho13)
      stop("`common.index` is TRUE and `alternative` is \"two.sided\" but `rho12` = `rho13`.", call. = FALSE)

    cor.mat <- matrix(c(1, rho12, rho13,
                           rho12, 1, rho23,
                           rho13, rho23, 1),
                         nrow = 3, ncol = 3)
    check.correlation.matrix(cor.mat)

    # common index
    if (pooled) {

      rho.bar.ab.ac <- (rho12 + rho13) / 2

      ## under null
      psi.ab.ac.0 <- rho23 * (1 - 2 * rho.bar.ab.ac ^ 2) - 0.50 * (rho.bar.ab.ac ^ 2) * (1 - 2 * rho.bar.ab.ac ^ 2 - rho23 ^ 2)
      cov.ab.ac.0 <- psi.ab.ac.0 / (1 - rho.bar.ab.ac ^ 2) ^ 2 # both = (rho12 + rho13) / 2 when pooled
      # sigma.ab.ac.0 <- sqrt((2 - 2 * cov.ab.ac.0) / (n - 3))

    } else {

      ## under null
      psi.ab.ac.0 <- rho23 * (1 - rho12 ^ 2 - rho12 ^ 2) - 0.50 * (rho12 * rho12) * (1 - rho12 ^ 2 - rho12 ^ 2 - rho23 ^ 2) # rho12 = rho13
      cov.ab.ac.0 <- psi.ab.ac.0 / ((1 - rho12 ^ 2) * (1 - rho12 ^ 2)) # rho12 = rho13
      # sigma.ab.ac.0 <- sqrt((2 - 2 * cov.ab.ac.0) / (n - 3))

    } # if pooled

    ## under alt
    psi.ab.ac.1 <- rho23 * (1 - rho12 ^ 2 - rho13 ^ 2) - 0.50 * (rho12 * rho13) * (1 - rho12 ^ 2 - rho13 ^ 2 - rho23 ^ 2)
    cov.ab.ac.1 <- psi.ab.ac.1 / ((1 - rho12 ^ 2) * (1 - rho13 ^ 2))
    # sigma.ab.ac.1 <- sqrt((2 - 2 * cov.ab.ac.1) / (n - 3))

    # z.ab <- cor.to.z(rho12)
    # z.ac <- cor.to.z(rho13)
    # sigma.ab.ac.0 <- sqrt((2 - 2 * cov.ab.ac.0) / (n - 3))
    # sigma.ab.ac.1 <- sqrt((2 - 2 * cov.ab.ac.1) / (n - 3))
    # lambda <- (z.ac - z.ab) /  sigma.ab.ac.0
    # sigma.lambda <- sigma.ab.ac.1 / sigma.ab.ac.0
    rho1 <- rho12
    rho2 <- rho13
    cov.null <- cov.ab.ac.0
    cov.alt <- cov.ab.ac.1

  } else {

    check.correlation(rho14, rho24, rho34)

    if (alternative == "two.sided" && rho12 == rho34)
      stop("`common.index` is FALSE and `alternative` = \"two.sided\" but `rho12` = `rho34`.", call. = FALSE)

    cor.mat <- matrix(c(1, rho12, rho13, rho14,
                             rho12, 1, rho23, rho24,
                             rho13, rho23, 1, rho34,
                             rho14, rho24, rho34, 1),
                           nrow = 4, ncol = 4)
    check.correlation.matrix(cor.mat)

    # no common index
    if (pooled) {

      rho.bar.ab.cd <- (rho12 + rho34) / 2

      ## under null
      psi.ab.cd.0 <- 0.50 * ((rho13 - rho.bar.ab.cd * rho23) * (rho24 - rho23 * rho.bar.ab.cd) +
                               (rho14 - rho13 * rho.bar.ab.cd) * (rho23 - rho.bar.ab.cd * rho13) +
                               (rho13 - rho14 * rho.bar.ab.cd) * (rho24 - rho.bar.ab.cd * rho14) +
                               (rho14 - rho.bar.ab.cd * rho24) * (rho23 - rho24 * rho.bar.ab.cd)) # rho12 = rho34
      cov.ab.cd.0 <- psi.ab.cd.0 / (1 - rho.bar.ab.cd ^ 2) ^ 2
      # sigma.ab.cd.0 <- sqrt((2 - 2 * cov.ab.cd.0) / (n - 3))

      ## under alt
      psi.ab.cd.1 <- 0.50 * ((rho13 - rho12 * rho23) * (rho24 - rho23 * rho34) +
                               (rho14 - rho13 * rho34) * (rho23 - rho12 * rho13) +
                               (rho13 - rho14 * rho34) * (rho24 - rho12 * rho14) +
                               (rho14 - rho12 * rho24) * (rho23 - rho24 * rho34))
      cov.ab.cd.1 <- psi.ab.cd.1 / ((1 - rho12 ^ 2) * (1 - rho34 ^ 2))
      # sigma.ab.cd.1 <- sqrt((2 - 2 * cov.ab.cd.1) / (n - 3))

    } else {

      ## under null
      psi.ab.cd.0 <- 0.50 * ((rho13 - rho12 * rho23) * (rho24 - rho23 * rho12) +
                               (rho14 - rho13 * rho12) * (rho23 - rho12 * rho13) +
                               (rho13 - rho14 * rho12) * (rho24 - rho12 * rho14) +
                               (rho14 - rho12 * rho24) * (rho23 - rho24 * rho12)) # rho12 = rho34
      cov.ab.cd.0 <- psi.ab.cd.0 / ((1 - rho12 ^ 2) * (1 - rho12 ^ 2))
      # sigma.ab.cd.0 <- sqrt((2 - 2 * cov.ab.cd.0) / (n - 3))

      ## under alt
      psi.ab.cd.1 <- 0.50 * ((rho13 - rho12 * rho23) * (rho24 - rho23 * rho34) +
                               (rho14 - rho13 * rho34) * (rho23 - rho12 * rho13) +
                               (rho13 - rho14 * rho34) * (rho24 - rho12 * rho14) +
                               (rho14 - rho12 * rho24) * (rho23 - rho24 * rho34))
      cov.ab.cd.1 <- psi.ab.cd.1 / ((1 - rho12 ^ 2) * (1 - rho34 ^ 2))
      # sigma.ab.cd.1 <- sqrt((2 - 2 * cov.ab.cd.1) / (n - 3))

    } # if pooled

    # z.ab <- cor.to.z(rho12)
    # z.cd <- cor.to.z(rho34)
    # sigma.ab.cd.0 <- sqrt((2 - 2 * cov.ab.cd.0) / (n - 3))
    # sigma.ab.cd.1 <- sqrt((2 - 2 * cov.ab.cd.1) / (n - 3))
    # lambda <- (z.cd - z.ab) /  sigma.ab.cd.0
    # sigma.lambda <- sigma.ab.cd.1 / sigma.ab.cd.0
    rho1 <- rho12
    rho2 <- rho34
    cov.null <- cov.ab.cd.0
    cov.alt <- cov.ab.cd.1

  } # if common.index

  if (requested == "n") {

    n <- ss.steiger(rho1 = rho1, rho2 = rho2, cov.null = cov.null, cov.alt = cov.alt,
                    power = power, alpha = alpha, alternative = alternative)

    if (ceiling) n <- ceiling(n)

  }

  # calculate power (if requested == "power") or update it (if requested == "n")
  pwr.obj <- pwr.steiger(rho1 = rho1, rho2 = rho2, cov.null = cov.null, cov.alt = cov.alt,
                         n = n, alpha = alpha, alternative = alternative)

  power <- pwr.obj$power
  mean.alternative <- ifelse(alternative == "two.sided" && rho1 - rho2 < 0, -pwr.obj$mean, pwr.obj$mean)
  sd.alternative <- pwr.obj$sd
  mean.null <- pwr.obj$null.mean
  sd.null <- pwr.obj$null.sd
  z.alpha <- pwr.obj$z.alpha

  delta <- rho1 - rho2
  q <- cors.to.q(rho1, rho2, FALSE)$q

  if (verbose > 0) {

    print.obj <-  list(requested = requested,
                       test = "Dependent Correlations",
                       design = "paired",
                       alpha = alpha,
                       alternative = alternative,
                       common = common.index,
                       delta = delta,
                       q = q,
                       mean.alternative = mean.alternative,
                       sd.alternative = sd.alternative,
                       mean.null = mean.null,
                       sd.null = sd.null,
                       z.alpha = z.alpha,
                       n = n,
                       power = power)

    .print.pwrss.steiger(print.obj, verbose = verbose, utf = utf)

  }

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           design = "paired",
                           delta = delta,
                           q = q,
                           mean = mean.alternative,
                           sd = sd.alternative,
                           null.mean = mean.null,
                           null.sd = sd.null,
                           alternative = alternative,
                           z.alpha = z.alpha,
                           n = n,
                           power = power),
                      class = c("pwrss", "z", "twocors", "paired")))

} # power.z.twocors.steiger()

#' @export power.z.steiger
power.z.steiger <- power.z.twocors.steiger


#' Power Analysis for Independent Correlations
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test
#' difference between two independent (Pearson) correlations using Fisher's z
#' transformation.
#'
#' Formulas are validated using PASS and G*Power.
#'
#' @aliases power.z.twocors power.z.twocor pwrss.z.2corrs pwrss.z.2corr
#'
#'
#' @param rho1        correlation in the first group.
#' @param rho2        correlation in the second group.
#' @param n.ratio     \code{n1 / n2} ratio.
#' @param n2          sample size in the second group. Sample size in the first
#'                    group can be calculated as \code{n2*kappa}. By default,
#'                    \code{n1 = n2} because \code{n.ratio = 1}.
#' @param power       statistical power, defined as the probability of
#'                    correctly rejecting a false null hypothesis, denoted as
#'                    \eqn{1 - \beta}.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided" or "one.sided".
#' @param ceiling     logical; whether sample size should be rounded up.
#'                    \code{TRUE} by default.
#' @param verbose     \code{1} by default (returns test, hypotheses, and
#'                    results), if \code{2} a more detailed output is given
#'                    (plus key parameters and definitions), if \code{0} no
#'                    output is printed on the console.
#' @param utf         logical; whether the output should show Unicode
#'                    characters (if encoding allows for it). \code{FALSE} by
#'                    default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (Z-Test)}
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{z.alpha}{critical value(s).}
#'   \item{power}{statistical power \eqn{(1-\beta)}}
#'   \item{n}{sample size for the first and second groups, in the form of
#'            c(n1, n2).}
#'
#' @references
#'   Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
#'   analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
#'   Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
#'   https://doi.org/10.29299/kefad.1209913
#'
#'   Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2018). Sample size
#'   calculations in clinical research (3rd ed.). Taylor & Francis / CRC.
#'
#'   Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'   (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @examples
#' # difference between r1 and r2 is different from zero
#' # it could be -0.10 as well as 0.10
#' power.z.twocors(rho1 = 0.20, rho2 = 0.30,
#'                power = 0.80, alpha = 0.05,
#'                alternative = "two.sided")
#'
#' # difference between r1 and r2 is greater than zero
#' power.z.twocors(rho1 = 0.30, rho2 = 0.20,
#'                power = 0.80, alpha = 0.05,
#'                alternative = "one.sided")
#'
#' @export power.z.twocors
power.z.twocors <- function(rho1, rho2,
                            n2 = NULL, n.ratio = 1,
                            power = NULL, alpha = 0.05,
                            alternative = c("two.sided", "one.sided"),
                            ceiling = TRUE, verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  func.parms <- clean.parms(as.list(environment()))

  check.correlation(rho1, rho2)
  if (!is.null(n2)) check.sample.size(n2)
  check.positive(n.ratio)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n2, power)

  z1 <- cor.to.z(rho1, FALSE)$z
  z2 <- cor.to.z(rho2, FALSE)$z

  if (requested == "n") {

    beta <- 1 - power
    if (alternative == "two.sided") {
      M <- stats::qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) + stats::qnorm(beta, mean = 0, sd = 1, lower.tail = FALSE)
      n2 <- stats::uniroot(function(n2) M ^ 2 - (z1 - z2) ^ 2 / (1 / (n.ratio * n2 - 3) + 1 / (n2 - 3)), interval = c(-1e10, 1e10))$root

    } else if (alternative == "one.sided") {
      M <- stats::qnorm(alpha,     mean = 0, sd = 1, lower.tail = FALSE) + stats::qnorm(beta, mean = 0, sd = 1, lower.tail = FALSE)
      n2 <- stats::uniroot(function(n2) M ^ 2 - (z1 - z2) ^ 2 / (1 / (n.ratio * n2 - 3) + 1 / (n2 - 3)), interval = c(0, 1e10))$root
    }

    n2 <- ifelse(ceiling, ceiling(n2), n2)
  }

  n1 <- ifelse(ceiling, ceiling(n.ratio * n2), n.ratio * n2)

  lambda <- (z1 - z2) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  if (alternative == "two.sided") {
    z.alpha <- stats::qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE) * c(-1, 1)
    power <- 1 - stats::pnorm(z.alpha[2],   mean = abs(lambda), sd = 1) + stats::pnorm(z.alpha[1], mean = abs(lambda), sd = 1)
  } else if (alternative == "one.sided") {
    z.alpha <- stats::qnorm(alpha,     mean = 0, sd = 1, lower.tail = FALSE) * ifelse(lambda < 0, -1, 1)
    power <- 1 - stats::pnorm(abs(z.alpha), mean = abs(lambda), sd = 1)
  }

  delta <- rho1 - rho2
  q <- cors.to.q(rho1, rho2, FALSE)$q

  mean.alternative <- lambda
  sd.alternative <- 1
  mean.null <- 0
  sd.null <- 1

  if (verbose > 0) {

    print.obj <-  list(requested = requested,
                       test = "Independent Correlations",
                       design = "independent",
                       alpha = alpha,
                       alternative = alternative,
                       delta = delta,
                       q = q,
                       mean.alternative = mean.alternative,
                       sd.alternative = sd.alternative,
                       mean.null = mean.null,
                       sd.null = sd.null,
                       z.alpha = z.alpha,
                       power = power,
                       n = c(n1 = n1, n2 = n2))

    .print.pwrss.twocors(print.obj, verbose = verbose, utf = utf)

  }

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           design = "independent",
                           delta = delta,
                           q = q,
                           mean = mean.alternative,
                           sd = sd.alternative,
                           null.mean = mean.null,
                           null.sd = sd.null,
                           alternative = alternative,
                           z.alpha = z.alpha,
                           n = c(n1 = n1, n2 = n2),
                           power = power),
                      class = c("pwrss", "z", "twocors", "independent")))

} # power.z.twocors

#' @export power.z.twocor
power.z.twocor <- power.z.twocors


#' @export pwrss.z.2corrs
pwrss.z.2corrs <- function(r1 = 0.50, r2 = 0.30,
                           alpha = 0.05, kappa = 1,
                           alternative = c("not equal", "greater", "less"),
                           n2 = NULL, power = NULL, verbose = TRUE) {

  alternative <- tolower(match.arg(alternative))
  verbose <- ensure_verbose(verbose)

  check.correlation(r1, r2)
  if (!is.null(power)) check.proportion(power)
  if (!is.null(n2)) check.sample.size(n2)

  if (alternative %in% c("less", "greater")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"

  twocors.obj <- power.z.twocors(rho1 = r1, rho2 = r2,
                             n2 = n2, n.ratio = kappa,
                             power = power, alpha = alpha,
                             alternative = alternative,
                             ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.twocors() function. \n")

  invisible(twocors.obj)

} # pwrss.z.2corrs()


##########################
# one correlation z test #
##########################

#' Power Analysis for One-Sample Correlation
#'
#' @description
#' Calculates power or sample size (only one can be NULL at a time) to test a
#' (Pearson) correlation against a constant using Fisher's z transformation.
#'
#' Formulas are validated using PASS and G*Power.
#'
#' @aliases power.z.onecor pwrss.z.corr pwrss.z.cor
#'
#'
#' @param rho         correlation.
#' @param null.rho    correlation when null is true.
#' @param n           sample size.
#' @param power       statistical power, defined as the probability of
#'                    correctly rejecting a false null hypothesis, denoted as
#'                    \eqn{1 - \beta}.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided" or "one.sided".
#' @param ceiling     logical; whether sample size should be rounded up.
#'                    \code{TRUE} by default.
#' @param verbose     \code{1} by default (returns test, hypotheses, and
#'                    results), if \code{2} a more detailed output is given
#'                    (plus key parameters and definitions), if \code{0} no
#'                    output is printed on the console.
#' @param utf         logical; whether the output should show Unicode
#'                    characters (if encoding allows for it). \code{FALSE} by
#'                    default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (Z-Test)}
#'   \item{mean}{mean of the alternative distribution.}
#'   \item{sd}{standard deviation of the alternative distribution.}
#'   \item{null.mean}{mean of the null distribution.}
#'   \item{null.sd}{standard deviation of the null distribution.}
#'   \item{z.alpha}{critical value(s).}
#'   \item{power}{statistical power\eqn{(1-\beta)}.}
#'   \item{n}{sample size.}
#'
#' @references
#'   Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel
#'   guc analizi \[Statistical power analysis with pwrss R package\]. *Ahi
#'   Evran Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
#'   https://doi.org/10.29299/kefad.1209913
#'
#'   Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2018). Sample size
#'   calculations in clinical research (3rd ed.). Taylor & Francis/CRC.
#'
#'   Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'   (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @examples
#' # expected correlation is 0.20 and it is different from 0
#' # it could be 0.20 as well as -0.20
#' power.z.onecor(rho = 0.20,
#'                power = 0.80,
#'                alpha = 0.05,
#'                alternative = "two.sided")
#'
#' # expected correlation is 0.20 and it is greater than 0.10
#' power.z.onecor(rho = 0.20, null.rho = 0.10,
#'                power = 0.80,
#'                alpha = 0.05,
#'                alternative = "one.sided")
#'
#'
#' @export power.z.onecor
power.z.onecor <- function(rho, null.rho = 0,
                           n = NULL, power = NULL, alpha = 0.05,
                           alternative = c("two.sided", "one.sided"),
                           ceiling = TRUE, verbose = 1, utf = FALSE) {

  alternative <- tolower(match.arg(alternative))
  func.parms <- clean.parms(as.list(environment()))

  check.correlation(rho, null.rho)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  check.proportion(alpha)
  check.logical(ceiling, utf)
  verbose <- ensure_verbose(verbose)
  requested <- check.n_power(n, power)

  z <- cor.to.z(rho, FALSE)$z
  null.z <- cor.to.z(null.rho, FALSE)$z

  if (requested == "n") {

    beta <- 1 - power
    if (alternative == "two.sided") {
      M <- stats::qnorm(alpha / 2, lower.tail = FALSE) + stats::qnorm(beta, lower.tail = FALSE)
      n <- M ^ 2 / (z - null.z) ^ 2 + 3
    } else if (alternative == "one.sided") {
      M <- stats::qnorm(alpha, lower.tail = FALSE) + stats::qnorm(beta, lower.tail = FALSE)
      n <- M ^ 2 / (z - null.z) ^ 2 + 3
    }

    if (ceiling) n <- ceiling(n)
  }

  lambda <- (z - null.z) / sqrt(1 / (n - 3))
  if (alternative == "two.sided") {
    z.alpha <- stats::qnorm(alpha / 2, lower.tail = FALSE) * c(-1, 1)
    power <- 1 - stats::pnorm(z.alpha[2], lambda) + stats::pnorm(z.alpha[1], lambda)
  } else if (alternative == "one.sided") {
    z.alpha <- stats::qnorm(alpha, lower.tail = FALSE)
    power <- 1 - stats::pnorm(z.alpha, abs(lambda))
  }

  delta <- rho - null.rho
  q <- cors.to.q(rho, null.rho, FALSE)$q

  mean.alternative <- lambda
  sd.alternative <- 1
  mean.null <- 0
  sd.null <- 1

  if (verbose > 0) {

    print.obj <-  list(requested = requested,
                       test = "One-Sample Correlation",
                       design = "one.sample",
                       alpha = alpha,
                       alternative = alternative,
                       delta = delta,
                       q = q,
                       mean.alternative = mean.alternative,
                       sd.alternative = sd.alternative,
                       mean.null = mean.null,
                       sd.null = sd.null,
                       z.alpha = z.alpha,
                       power = power,
                       n = n)

    .print.pwrss.twocors(print.obj, verbose = verbose, utf = utf)

  }

  invisible(structure(list(parms = func.parms,
                           test = "z",
                           design = "one.sample",
                           delta = delta,
                           q = q,
                           mean = mean.alternative,
                           sd = sd.alternative,
                           null.mean = mean.null,
                           null.sd = sd.null,
                           alternative = alternative,
                           z.alpha = z.alpha,
                           n = n,
                           power = power),
                      class = c("pwrss", "z", "onecor")))

} # power.z.onecor()


#' @export pwrss.z.corr
pwrss.z.corr <- function(r = 0.50, r0 = 0, alpha = 0.05,
                         alternative = c("not equal", "greater", "less"),
                         n = NULL, power = NULL, verbose = TRUE) {

  alternative <- tolower(match.arg(alternative))
  verbose <- ensure_verbose(verbose)

  check.correlation(r, r0)
  if (!is.null(power)) check.proportion(power)
  if (!is.null(n)) check.sample.size(n)

  if (alternative %in% c("less", "greater")) alternative <- "one.sided"
  if (alternative == "not equal") alternative <- "two.sided"

  onecor.obj <- power.z.onecor(rho = r, null.rho = r0,
                               n = n, power = power, alpha = alpha,
                               alternative = alternative,
                               ceiling = TRUE, verbose = verbose)

  # cat("This function will be removed in the future. \n Please use power.z.onecor() function. \n")

  invisible(onecor.obj)

} # pwrss.z.corr()

##############################
# one correlation exact test #
##############################

#' Power Analysis for One-Sample Correlation (Exact)
#'
#' @description
#' Calculates power, sample size, or minimum detectable correlation (only one can be NULL at a time) to test a
#' (Pearson) correlation against a constant using exact method described in Barabesi and Greco (2002).
#'
#' Formulas are validated using G*Power.
#'
#' @aliases power.exact.onecor 
#'
#'
#' @param rho         correlation.
#' @param null.rho    correlation when null is true. Only 0 is allowed for now.
#' @param n           sample size.
#' @param power       statistical power, defined as the probability of
#'                    correctly rejecting a false null hypothesis, denoted as
#'                    \eqn{1 - \beta}.
#' @param alpha       type 1 error rate, defined as the probability of
#'                    incorrectly rejecting a true null hypothesis, denoted as
#'                    \eqn{\alpha}.
#' @param alternative character; the direction or type of the hypothesis test:
#'                    "two.sided" or "one.sided".
#' @param verbose     \code{1} by default (returns test, hypotheses, and
#'                    results), if \code{2} a more detailed output is given
#'                    (plus key parameters and definitions), if \code{0} no
#'                    output is printed on the console.
#' @param utf         logical; whether the output should show Unicode
#'                    characters (if encoding allows for it). \code{FALSE} by
#'                    default.
#'
#' @return
#'   \item{parms}{list of parameters used in calculation.}
#'   \item{test}{type of the statistical test (Z-Test)}
#'   \item{rho.alpha}{critical value(s).}
#'   \item{es}{minimum detectable correlation.}
#'   \item{power}{statistical power\eqn{(1-\beta)}.}
#'   \item{n}{sample size.}
#'
#' @references
#'   Barabesi & Greco (2002): A note on the exact computation of the Student t, 
#'   Snecdor F and sample correlation coefficient distribution function. 
#'   The Statistician, 51(Part 1), 105-110. 
#'
#'
#' @examples
#' # expected correlation is 0.20 and it is different from 0
#' # it could be 0.20 as well as -0.20
#' power.exact.onecor(rho = 0.20,
#'                power = 0.80,
#'                alpha = 0.05,
#'                alternative = "two.sided")
#'
#' # expected correlation is 0.20 and it is greater than 0
#' power.exact.onecor(rho = 0.20, null.rho = 0.10,
#'                power = 0.80,
#'                alpha = 0.05,
#'                alternative = "one.sided")
#'
#'
#' @export power.exact.onecor
power.exact.onecor <- function(rho = NULL, null.rho = 0,
                               n = NULL, n.max = 500, power = NULL, alpha = 0.05,
                               alternative = c("two.sided", "one.sided"),
                               verbose = 1, utf = FALSE) {
  
  alternative <- tolower(match.arg(alternative))
  func.parms <- clean.parms(as.list(environment()))
  
  if (null.rho != 0) stop("'null.rho' cannot be different from 0 at the moment", call. = FALSE)
  if (!is.null(rho)) check.correlation(rho, null.rho)
  if (!is.null(n)) check.sample.size(n)
  if (!is.null(power)) check.proportion(power)
  if(sum(c(is.null(rho), is.null(n), is.null(power))) > 1) 
    stop("Excatly one of the 'rho', 'power', and 'n' can be NULL", call. = FALSE)
  
  check.proportion(alpha)
  check.logical(utf)
  verbose <- ensure_verbose(verbose)
  
  if(is.null(rho)) requested <- "es"
  if(is.null(n)) requested <- "n"
  if(is.null(power)) requested <- "power"
  
  # functions 
  # step 1: L recursion
  L.seq <- function(r, rho, K.max) {
    if (abs(r) >= 1) stop("L.seq: need |r| < 1.")
    if (abs(rho) >= 1) stop("L.seq: need |rho| < 1.")
    if (K.max < 0) stop("L.seq: K.max must be >= 0.")
    
    c1 <- 1 - (r^2) * (rho^2)                
    c2 <- (1 - r^2) * (1 - rho^2)             
    
    if (c1 < 0 && c1 > -1e-14) c1 <- 0
    if (c2 < 0 && c2 > -1e-14) c2 <- 0
    if (c1 <= 0) stop("L.seq: c1 <= 0 (numerical instability).")
    
    L <- numeric(K.max + 1)
    
    # L0
    L[1] <- acos(-r * rho) / sqrt(c1)
    
    # L1
    if (K.max >= 1) {
      L[2] <- (sqrt(c2) / c1) * (r * rho * L[1] + 1)
    }
    
    # recursion for k >= 2 
    if (K.max >= 2) {
      for (k in 2:K.max) {
        L[k + 1] <- ((2 * k - 1) / k) * (r * rho * sqrt(c2) / c1) * L[k] + # L_{k-1}
          ((k - 1) / k) * (c2 / c1) * L[k - 1] # L_{k-2}
      }
    }
    
    return(L) # L[1]=L0, L[2]=L1, ...
    
  }
  
  # step 2: exact CDF F
  FR.exact <- function(r, rho, n) {
    
    if (n < 3) stop("FR.exact: need n >= 3.")
    if (abs(r) >= 1) stop("FR.exact: need |r| < 1.")
    if (abs(rho) >= 1) stop("FR.exact: need |rho| < 1.")
    
    K.max <- max(n - 3, 1)
    L <- L.seq(r, rho, K.max)
    
    if (n %% 2 == 0) {
      # n even:
      upper.even <- n/2 - 2
      upper.odd  <- n/2 - 1
      
      sum.even <- 0
      if (upper.even >= 0) {
        idx <- 0:upper.even
        sum.even <- sum(L[2 * idx + 1])   # L_{2i} -> 2i+1
      }
      
      sum.odd <- 0
      if (upper.odd >= 1) {
        idx <- 1:upper.odd
        sum.odd <- sum(L[2 * idx])        # L_{2i-1} -> 2i
      }
      
      out <- (1/pi) * (
        acos(rho) +
          r * sqrt(1 - rho^2) * sum.even -
          rho * sqrt(1 - r^2) * sum.odd
      )
      
    } else {
      # n odd:
      upper <- (n - 3) / 2
      
      sum.odd <- 0
      if (upper >= 1) {
        idx <- 1:upper
        sum.odd <- sum(L[2 * idx])        # L_{2i-1} -> 2i
      }
      
      sum.even <- 0
      if (upper >= 0) {
        idx <- 0:upper
        sum.even <- sum(L[2 * idx + 1])   # L_{2i} -> 2i+1
      }
      
      out <- (1/pi) * (
        acos(-r) +
          r * sqrt(1 - rho^2) * sum.odd -
          rho * sqrt(1 - r^2) * sum.even
      )
    }
    
    min(max(out, 0), 1)
  }
  
  # step 3: exact critical values 
  crit.r.two.sided <- function(n, alpha = 0.05) {
    target <- function(crit) {
      FR.exact(-crit, rho = 0, n = n) + (1 - FR.exact(crit, rho = 0, n = n)) - alpha
    }
    uniroot(target, lower = 1e-10, upper = 1 - 1e-10)$root
  }
  
  crit.r.one.sided.upper <- function(n, alpha = 0.05) {
    target <- function(crit) (1 - FR.exact(crit, rho = 0, n = n)) - alpha
    uniroot(target, lower = 1e-10, upper = 1 - 1e-10)$root
  }
  
  crit.r.one.sided.lower <- function(n, alpha = 0.05) {
    target <- function(crit) FR.exact(crit, rho = 0, n = n) - alpha
    uniroot(target, lower = -1 + 1e-10, upper = -1e-10)$root
  }
  
  # power
  pwr.exact.rho <- function(n, rho, alpha = 0.05,
                            alternative = c("two.sided", "greater", "less")) {
    alternative <- match.arg(alternative)
    
    if (alternative == "two.sided") {
      crit <- crit.r.two.sided(n, alpha)
      pwr <- FR.exact(-crit, rho = rho, n = n) + (1 - FR.exact(crit, rho = rho, n = n))
    }
    
    if (alternative == "greater") {
      crit <- crit.r.one.sided.upper(n, alpha)
      pwr <- 1 - FR.exact(crit, rho = rho, n = n)
    }
    
    if (alternative == "less") {
      crit <- crit.r.one.sided.lower(n, alpha)
      pwr <- FR.exact(crit, rho = rho, n = n)
    }
    
    return(list(power = pwr, crit = crit, alternative = alternative,
                alpha = alpha, n = n, rho = rho))
    
  } #  pwr.exact.rho
  
  # minimum sample size
  ss.exact.rho <- function(rho, alpha = 0.05, power = 0.80,
                           alternative = c("two.sided", "greater", "less"),
                           n.min = 3, n.max = 500,
                           verbose = TRUE) {
    alternative <- match.arg(alternative)
    if (n.min < 3) n.min <- 3
    if (n.max < n.min) stop("ss.exact.rho: n.max must be >= n.min.")
    if (abs(rho) >= 1) stop("ss.exact.rho: need |rho| < 1.")
    if (!(power > 0 && power < 1)) stop("power must be in (0,1).")
    
    pwr.at.n <- function(n) pwr.exact.rho(n, rho, alpha, alternative)$power
    
    # check bounds
    pwr.min <- pwr.at.n(n.min)
    if (verbose) message(sprintf("Power at n.min=%d: %.4f", n.min, pwr.min))
    if (pwr.min >= power) {
      return(list(n = n.min, power = pwr.min, achieved = TRUE,
                  alpha = alpha, rho = rho, power = power,
                  alternative = alternative))
    }
    
    pwr.max <- pwr.at.n(n.max)
    if (verbose) message(sprintf("Power at n.max=%d: %.4f", n.max, pwr.max))
    if (pwr.max < power) {
      return(list(n = n.max, power = pwr.max, achieved = FALSE,
                  alpha = alpha, rho = rho, power = power,
                  alternative = alternative,
                  message = "Target power not reached within [n.min, n.max]. Increase n.max."))
    }
    
    # bisection on n
    low <- n.min
    high <- n.max
    while (low + 1 < high) {
      mid <- floor((low + high) / 2)
      pwr.mid <- pwr.at.n(mid)
      if (verbose) message(sprintf("n=%d -> power=%.4f", mid, pwr.mid))
      if (pwr.mid >= power) {
        high <- mid
      } else {
        low <- mid
      }
    }
    
    final <- pwr.exact.rho(high, rho, alpha, alternative)
    list(n = high, power = final$power, achieved = (final$power >= power),
         crit = final$crit,
         alpha = alpha, rho = rho, power = power,
         alternative = alternative)
    
  } #  ss.exact.rho
  
  # minimum detectable rho for target power 
  mde.exact.rho <- function(n, alpha = 0.05, power = 0.80,
                            alternative = c("two.sided", "greater", "less"),
                            rho.min = 1e-6, rho.max = 0.999,
                            tol = 1e-6, max.iter = 100,
                            verbose = TRUE) {
    
    alternative <- match.arg(alternative)
    if (n < 3) stop("mde.exact.rho: need n >= 3.")
    if (!(power > 0 && power < 1)) stop("power must be in (0,1).")
    if (!(alpha > 0 && alpha < 1)) stop("alpha must be in (0,1).")
    if (!(rho.min > 0 && rho.min < 1)) stop("rho.min must be in (0,1).")
    if (!(rho.max > 0 && rho.max < 1)) stop("rho.max must be in (0,1).")
    if (rho.max <= rho.min) stop("rho.max must be > rho.min.")
    
    # direction for one-sided tests
    if (alternative == "greater") {
      sign.rho <-  1
    } else if (alternative == "less") {
      sign.rho <- -1
    } else {
      sign.rho <-  1  
    }
    
    # power as a function of |rho| (two-sided) or rho with sign (one-sided)
    pwr.at.rho <- function(rho.abs) {
      rho.use <- if (alternative == "two.sided") rho.abs else sign.rho * rho.abs
      pwr.exact.rho(n = n, rho = rho.use, alpha = alpha, alternative = alternative)$power
    }
    
    # check bounds
    pwr.min <- pwr.at.rho(rho.min)
    if (verbose) message(sprintf("Power at rho.min=%.6f: %.4f", rho.min, pwr.min))
    if (pwr.min >= power) {
      rho.use <- if (alternative == "two.sided") rho.min else sign.rho * rho.min
      return(list(rho = rho.use, abs.rho = rho.min, power = pwr.min, achieved = TRUE,
                  n = n, alpha = alpha, power = power,
                  alternative = alternative))
    }
    
    pwr.max <- pwr.at.rho(rho.max)
    if (verbose) message(sprintf("Power at rho.max=%.6f: %.4f", rho.max, pwr.max))
    if (pwr.max < power) {
      rho.use <- if (alternative == "two.sided") rho.max else sign.rho * rho.max
      return(list(rho = rho.use, abs.rho = rho.max, power = pwr.max, achieved = FALSE,
                  n = n, alpha = alpha, power = power,
                  alternative = alternative,
                  message = "Target power not reached within [rho.min, rho.max]. Increase rho.max (closer to 1)."))
    }
    
    # bisection on |rho|
    low <- rho.min
    high <- rho.max
    iter <- 0
    while ((high - low) > tol && iter < max.iter) {
      iter <- iter + 1
      mid <- (low + high) / 2
      pwr.mid <- pwr.at.rho(mid)
      if (verbose) message(sprintf("iter=%d, |rho|=%.6f -> power=%.4f", iter, mid, pwr.mid))
      if (pwr.mid >= power) {
        high <- mid
      } else {
        low <- mid
      }
    }
    
    rho.abs <- high
    rho.use <- ifelse(alternative == "two.sided", rho.abs, sign.rho * rho.abs)
    pwr.final <- pwr.at.rho(rho.abs)
    
    list(rho = rho.use, abs.rho = rho.abs, power = pwr.final, achieved = (pwr.final >= power),
         n = n, alpha = alpha, power = power,
         alternative = alternative, iterations = iter, tol = tol)
    
  } #  mde.exact.rho
  
  
  # calculate requested parameter 
  if(requested == "power") {
    if(sign(rho) == -1 & alternative == "one.sided") alternative <- "less"
    if(sign(rho) == 1 & alternative == "one.sided") alternative <- "greater"
    pwr.obj <- pwr.exact.rho(n = n, rho = rho, alpha = alpha,
                             alternative = alternative)
  }
  
  if(requested == "n") {
    if(sign(rho) == -1 & alternative == "one.sided") alternative <- "less"
    if(sign(rho) == 1 & alternative == "one.sided") alternative <- "greater"
    n <- ss.exact.rho(rho = rho, alpha = alpha, power = power,
                      alternative = alternative,
                      n.min = 3, n.max = n.max,
                      verbose = TRUE)$n
    pwr.obj <- pwr.exact.rho(n = n, rho = rho, alpha = alpha,
                             alternative = alternative)
  }
  
  if(requested == "es") {
    rho <- mde.exact.rho(n = n, alpha = alpha, power = power,
                         alternative = alternative,
                         verbose = FALSE)$rho
    pwr.obj <- pwr.exact.rho(n = n, rho = rho, alpha = alpha,
                             alternative = alternative)
  }
  
  power <- pwr.obj$power
  rho.alpha <- pwr.obj$crit
  
  delta <- rho - null.rho
  q <- cors.to.q(rho, null.rho, FALSE)$q
  
  #### for compatibility ####
  if(alternative %in% c("less", "greater")) alternative <- "one.sided"
  mean.alternative <- NA
  sd.alternative <- NA
  mean.null <- NA
  sd.null <- NA
  z.alpha <- NA
  #### for compatibility ####
  
  if (verbose > 0) {
    
    print.obj <-  list(requested = requested,
                       test = "One-Sample Correlation (Exact)",
                       design = "one.sample",
                       alpha = alpha,
                       alternative = alternative,
                       delta = delta,
                       q = q,
                       
                       #### for compatibility ####
                       mean.alternative = mean.alternative,
                       sd.alternative = sd.alternative,
                       mean.null = mean.null,
                       sd.null = sd.null,
                       z.alpha = z.alpha,
                       #### for compatibility ####
                       
                       rho.alpha = rho.alpha,
                       es = rho,
                       power = power,
                       n = n)
    
    .print.pwrss.twocors(print.obj, verbose = verbose, utf = utf)
    
  }
  
  invisible(structure(list(parms = func.parms,
                           test = "exact",
                           design = "one.sample",
                           delta = delta,
                           q = q,
                           alternative = alternative,
                           rho.alpha = rho.alpha, # critical value for cor
                           es = rho,
                           n = n,
                           power = power),
                      class = c("pwrss", "exact", "onecor")))
  
} # power.exact.onecor
