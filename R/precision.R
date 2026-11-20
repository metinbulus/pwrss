# double check how equivalence and minimum effect tests are handled since they have two null values
precision.power <- function(x, probs = c(0.025, 0.50, 0.975), verbose = 1, utf = FALSE) {
  if (verbose > 0 && length(probs) > 10)
    stop("Printing is not allowed with large output. Try verbose = 0.", call. = FALSE)

  utf <- utf || ifelse("generic" %in% class(x), x$utf, x$parms$utf)
  if ("binom" %in% class(x)) {
    prec.obj <- .precision.power.binom(x, probs = probs)
  } else if ("chisq" %in% class(x)) {
    prec.obj <- .precision.power.chisq(x, probs = probs)
  } else if ("exact" %in% class(x)) {
    stop("Uncertainty quantiles are not available for exact tests at the moment.", call. = TRUE)
  } else if ("f" %in% class(x)) {
    prec.obj <- .precision.power.f(x, probs = probs)
  } else if ("lp" %in% class(x)) {
    prec.obj <- .precision.power.lp(x, probs = probs)
  } else if ("t" %in% class(x)) {
    prec.obj <- .precision.power.t(x, probs = probs)
  } else if ("z" %in% class(x)) {
    prec.obj <- .precision.power.z(x, probs = probs)
  } else {
    stop("The object `x` should inherit class 'pwrss' and either 't', 'z', 'lp', 'f', 'chisq', or 'binom'.", call. = FALSE)
  }

  if (verbose > 0) .print.precision(prec.obj, verbose = verbose, utf = utf, class = class(x))

  prec.obj <- rbind(as.numeric(gsub("%", "", colnames(prec.obj))) / 100, prec.obj)
  rownames(prec.obj) <- .get_rn(class(x))
  colnames(prec.obj) <- NULL

  invisible(as.data.frame(t(prec.obj)))

} # precision.power

precision <- precision.power


# ======================================================================================================================
# calculation functions for the individual distributions (binom, chisq, f, lp, t, z)
# ----------------------------------------------------------------------------------------------------------------------
# .precision.power.binom
.precision.power.binom <- function(x, probs = c(0.025, 0.50, 0.975)) {

  if (!all(c("pwrss", "binom") %in% class(x))) stop("Object should be a class of 'pwrss' and 'binom'", call. = FALSE)

  conf.int <- precision.binom.test(statistic = x$prob, size = x$size, probs = probs)$ci
  if (any(is.na(conf.int)))
    stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)

  power <- vapply(conf.int,
                  function(prob) {
                    tryCatch(power.binom.test(prob = prob, null.prob = x$null.prob, size = x$size,
                                              alternative = x$alternative, verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                  numeric(1L))

  prec.obj <- data.frame(rbind(conf.int, power))
  rownames(prec.obj) <- c("  Prop. Uncertainty Distribution", "  Conditional Statistical Power")
  colnames(prec.obj) <- paste0(round(probs * 100, 1), "%")

  invisible(prec.obj)

} # .precision.power.binom()
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# .precision.power.chisq
.precision.power.chisq <- function(x, probs = c(0.025, 0.50, 0.975)) {

  if (!all(c("pwrss", "chisq") %in% class(x))) stop("Object should be a class of 'pwrss' and 'chisq'", call. = FALSE)

  conf.int <- precision.chisq.test(statistic = x$ncp, df = x$df, probs = probs)$ci
  if (any(is.na(conf.int)) || all(conf.int <= 0))
    stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)
  conf.int[conf.int <= 0] <- 1e-12

  power <- vapply(conf.int,
                  function(ncp) {
                    tryCatch(power.chisq.test(ncp = ncp, null.ncp = x$null.ncp, df = x$df, verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                  numeric(1L))

  prec.obj <- data.frame(rbind(conf.int, power))
  rownames(prec.obj) <- c("  NCP Uncertainty Distribution", "  Conditional Statistical Power")
  colnames(prec.obj) <- paste0(round(probs * 100, 1), "%")

  invisible(prec.obj)

} # .precision.power.chisq
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# .precision.power.f
.precision.power.f <- function(x, probs = c(0.025, 0.50, 0.975)) {

  if (!all(c("pwrss", "f") %in% class(x))) stop("Object should be a class of 'pwrss' and 'f'", call. = FALSE)

  if ("generic" %in% class(x)) {
    effect <- "  Cohen's f-squared"
    f.statistic <- (x$ncp / ((x$df1 + x$df2 + 1))) * (x$df2 / x$df1)
  } else if ("anova_mixed" %in% class(x)) {
    effect <- "  (Partial) Eta-squared"
    f.statistic <- (x$ncp / (x$n.total * x$parms$epsilon)) * (x$df2 / x$df1)
  } else if ("regression" %in% class(x)) {
    effect <- "  (Delta) R-squared"
    f.statistic <- (x$ncp / x$n) * (x$df2 / x$df1)
  } else if ("ancova" %in% class(x)) {
    effect <- "  Eta-squared"
    f.statistic <- (x$ncp / x$n.total) * (x$df2 / x$df1)
  }

  conf.int <- precision.f.test(statistic = f.statistic, df1 = x$df1, df2 = x$df2, probs = probs)$ci
  if (any(is.na(conf.int)) || all(conf.int <= 0))
    stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)
  conf.int[conf.int <= 0] <- 1e-12
  f.squared.ci <- conf.int / x$df2

  power <- vapply(conf.int,
                  function(ncp) {
                    tryCatch(power.f.test(ncp = ncp, null.ncp = x$null.ncp, df1 = x$df1, df2 = x$df2, verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                  numeric(1L))

  prec.obj <- data.frame(rbind(conf.int, if ("generic" %in% class(x)) f.squared.ci else f.squared.ci / (1 + f.squared.ci), power))
  colnames(prec.obj) <-  paste0(round(probs * 100, 1), "%")
  rownames(prec.obj) <- c("  NCP Uncertainty Distribution", effect, "  Conditional Statistical Power")

  invisible(prec.obj)

} # .precision.power.f()
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# .precision.power.lp
.precision.power.lp <- function(x, probs = c(0.025, 0.50, 0.975)) {

  if (!all(c("pwrss", "lp") %in% class(x))) stop("Object should be a class of 'pwrss' and 'lp'", call. = FALSE)

  conf.int <- precision.lp.test(statistic = x$ncp, df = x$df, probs = probs)$ci

  if (any(is.na(conf.int)))
    stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)

  power <- vapply(conf.int,
                  function(ncp) {
                    tryCatch(power.lp.test(ncp = ncp, null.ncp = x$null.ncp, alternative = x$alternative, df = x$df,
                                           verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                  numeric(1L))

  prec.obj <- data.frame(rbind(conf.int, power))
  colnames(prec.obj) <-  paste0(round(probs * 100, 1), "%")
  rownames(prec.obj) <- c("  NCP Uncertainty Distribution", "  Conditional Statistical Power")

  invisible(prec.obj)

} # .precision.power.lp()
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# .precision.power.t
.precision.power.t <- function(x, probs = c(0.025, 0.50, 0.975)) {

  if (!all(c("pwrss", "t") %in% class(x))) stop("Object should be a class of 'pwrss' and 't'", call. = FALSE)

  conf.int <- precision.t.test(statistic = x$ncp, df = x$df, probs = probs)$ci
  if (any(is.na(conf.int)))
    stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)

  power <- vapply(conf.int,
                  function(ncp) {
                    tryCatch(power.t.test(ncp = ncp, null.ncp = x$null.ncp, alternative = x$alternative, df = x$df,
                                          verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_)
                  },
                  numeric(1L))

  if ("generic" %in% class(x)) {

    effect <- "  t-value"
    prec.obj <- data.frame(rbind(conf.int, conf.int, power))

  } else if ("welch" %in% class(x)) { # generic -> welch

    effect <- "  Cohen's d"

    # variance ratio constraint
    sd2 <- sqrt((x$n[2] * (x$parms$n.ratio + 1) - 2) / (x$n[2] - 1 + x$parms$var.ratio * (x$n[2] * x$parms$n.ratio - 1)))
    sd1 <- sqrt(x$parms$var.ratio * sd2 ^ 2)
    var.delta <- 1 / x$n[1] * sd1 ^ 2 + 1 / x$n[2] * sd2 ^ 2
    std <- sqrt(((x$n[1] - 1) * sd1 ^ 2 + (x$n[2] - 1) * sd2 ^ 2) / (x$n[1] + x$n[2] - 2))
    var.std <- ((x$n[1] - 1) * sd1 ^ 4 + (x$n[2] - 1) * sd2 ^ 4) /
               ((2 * (x$n[1] - 1) * sd1 ^ 2 + 2 * (x$n[2] - 1) * sd2 ^ 2) * (x$n[1] + x$n[2] - 2))

    if (x$parms$claim.basis == "md.pval") {
      d <- conf.int * sqrt(var.delta) + x$parms$null.d
    } else {
      disc <- (-2 * x$parms$null.d) ^ 2 - 4 * (1 - conf.int ^ 2 * (var.std   / std ^ 2)) *
              (x$parms$null.d ^ 2 - conf.int ^ 2 * (var.delta / std ^ 2))
      if (any(disc < 0)) {
        warning("No real solution for some d.", call. = FALSE)
        disc[disc < 0] <- NA_real_
      }
      d1 <- (2 * x$parms$null.d + sqrt(disc)) / (2 * (1 - conf.int ^ 2 * (var.std   / std ^ 2)))
      d2 <- (2 * x$parms$null.d - sqrt(disc)) / (2 * (1 - conf.int ^ 2 * (var.std   / std ^ 2)))
      d <- ifelse(sign(d1) == sign(conf.int), d1, d2)
    }

    prec.obj <- data.frame(rbind(conf.int, d, power))

  } else if ("student" %in% class(x)) { # welch -> student

    effect <- "  Cohen's d"

    if (x$parms$design == "independent") {

      if (x$parms$claim.basis == "md.pval") {
        var.delta <- (x$n[1] + x$n[2]) / (x$n[1] * x$n[2])
        d <- conf.int * sqrt(var.delta) + x$parms$null.d
      } else {
        disc <- (-2 * x$parms$null.d) ^ 2 - 4 * (1 - (conf.int ^ 2) * (1 / (2 * x$df))) *
                (x$parms$null.d ^ 2 - (conf.int ^ 2) * ((x$n[1] + x$n[2]) / (x$n[1] * x$n[2])))
        if (any(disc < 0)) {
          warning("No real solution for some d.", call. = FALSE)
          disc[disc < 0] <- NA_real_
        }
        d1 <- (2 * x$parms$null.d + sqrt(disc)) / (2 * (1 - (conf.int ^ 2) * (1 / (2 * x$df))))
        d2 <- (2 * x$parms$null.d - sqrt(disc)) / (2 * (1 - (conf.int ^ 2) * (1 / (2 * x$df))))
        d <- ifelse(sign(d1) == sign(conf.int), d1, d2)
      }

    } else {

      if (x$parms$claim.basis == "md.pval") {
        var.delta <- 1 / x$n
        d <- conf.int * sqrt(var.delta) + x$parms$null.d
      } else {
        disc <- (-2 * x$parms$null.d) ^ 2 - 4 * (1 - (conf.int ^ 2) * (1 / (2 * x$n))) *
                (x$parms$null.d ^ 2 - (conf.int ^ 2) * (1 / x$n))
        if (any(disc < 0)) {
          warning("No real solution for some d.", call. = FALSE)
          disc[disc < 0] <- NA_real_
        }
        d1 <- (2 * x$parms$null.d  + sqrt(disc)) / (2 * (1 - (conf.int ^ 2) * (1 / (2 * x$n))))
        d2 <- (2 * x$parms$null.d  - sqrt(disc)) / (2 * (1 - (conf.int ^ 2) * (1 / (2 * x$n))))
        d <- ifelse(sign(d1) == sign(conf.int), d1, d2)
      }

    } # independent?

    prec.obj <- data.frame(rbind(conf.int, d, power))

  } else if ("regression" %in% class(x)) { # student -> regression

    effect <- c("  Coefficient (Beta)", "  Standardized Beta")

    if (is.null(x$parms$r.squared)) stop("R-squared cannot be NULL with precision calculations.", call. = FALSE)

    # lambda <- (beta - x$parms$null.beta)  / ((x$parms$sd.outcome / x$parms$sd.predictor) * sqrt((1 - x$r.squared) / x$n))
    beta <- conf.int * ((x$parms$sd.outcome / x$parms$sd.predictor) * sqrt((1 - x$r.squared) / x$n)) + x$parms$null.beta
    std.beta <- beta * (x$parms$sd.predictor / x$parms$sd.outcome)

    prec.obj <- data.frame(rbind(conf.int, beta, std.beta, power))

  }  else if ("wilcoxon" %in% class(x)) {  # regression -> wilcoxon

    effect <- c("  Cohen's d", "  Common Language Effect Size")

    if (x$method != "guenther") stop("Only `method` == 'guenther' is valid here.", call. = FALSE)

    d <- conf.int * sqrt(ifelse(x$parms$design == "independent", 1 / x$n[1] + 1 / x$n[2], 1 / x$n)) + x$parms$null.d
    cles <- vapply(d, function(di) d.to.cles(d = di, design = x$parms$design, verbose = 0)$cles, numeric(1))

    prec.obj <- data.frame(rbind(conf.int, d, cles, power))

  } # wilcoxon

  rownames(prec.obj) <- c("  NCP Uncertainty Distribution", effect, "  Conditional Statistical Power")
  colnames(prec.obj) <- paste0(round(probs * 100, 1), "%")

  invisible(prec.obj)

} # .precision.power.t()
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------------------------
# .precision.power.z
.precision.power.z <- function(x, probs = c(0.025, 0.50, 0.975)) {

  if (!all(c("pwrss", "z") %in% class(x))) stop("Object should be a class of 'pwrss' and 'z'", call. = FALSE)

  conf.int <- precision.z.test(statistic = x$mean, probs = probs)$ci
  if (any(is.na(conf.int)))
    stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)

  if ("generic" %in% class(x)) {  # generic

    effect <- "  z-value"
    prec.obj <- data.frame(rbind(conf.int, conf.int))

  } else if ("wilcoxon" %in% class(x)) {  # generic -> wilcoxon

    effect <- c("  Cohen's d", "  Common Language Effect Size")

    if (x$method != "noether") stop("Only `method` == 'noether' is valid here.", call. = FALSE)

    null.prob <- d.to.cles(d = x$parms$null.d, design = x$parms$design, verbose = 0)$cles
    cles <- null.prob + conf.int /
      sqrt(12 * ifelse(x$parms$design == "independent", x$n[2], x$n) * x$parms$n.ratio / (1 + x$parms$n.ratio))
    d <- vapply(cles, function(clesi) cles.to.d(cles = clesi, design = x$parms$design, verbose = 0)$d, numeric(1))

    prec.obj <- data.frame(rbind(conf.int, d, cles))

  } else if ("oneprop" %in% class(x)) {  # wilcoxon -> oneprop

    effect <- c("  Proportion", "  Cohen's h")

    h <- props <- null.sd <- numeric(length(conf.int))

    null.prob <- switch(x$alternative,
                        two.one.sided = ifelse(all(sign(x$null.mean) == 1), min(x$null.prob), max(x$null.prob)),
                        x$null.prob)

    for (i in seq_along(conf.int)) {

      if (x$alternative == "two.one.sided") {

        if (all(sign(x$null.mean) == -1)) {
          z <- conf.int[i] + abs(max(x$null.mean))
        } else if (all(sign(x$null.mean) == 1)) {
          z <- conf.int[i] - min(x$null.mean)
        } else {
          z <- conf.int[i] - x$null.mean[sign(x$null.mean) == 1]
        }

      } else {

        z <- conf.int[i]

      } # two.one.sided

      zp.obj <- z.to.prob(z = z, null.prob = null.prob, n = x$n, alternative = x$alternative, std.error = x$std.error,
                          arcsine = x$parms$arcsine, correct = x$parms$correct)

      props[i] <- zp.obj$prob
      null.sd[i] <- zp.obj$null.sd
      h[i] <- probs.to.h(prob1 = zp.obj$prob, prob2 = null.prob, verbose = 0)$h

    } # for loop

    prec.obj <- data.frame(rbind(conf.int, props, h))

  } else if ("logistic" %in% class(x)) {  # oneprop -> logistic

    effect <- c("  Coefficient (Beta)", "  Odds Ratio")

    if (x$parms$method == "hsieh") stop("Precision calculations for Hsieh method are not supported at the moment.", call. = FALSE)

    var.obj <- var.beta.logistic(beta0 = x$beta0, beta1 = x$beta1, distribution = x$distribution)

    beta1 <- conf.int * sqrt(var.obj$var.beta1 / (x$n * (1 - x$parms$r.squared.predictor)))
    odds.ratio <- exp(beta1)

    ncp <- numeric(length(conf.int))
    sd.ncp <- numeric(length(conf.int))
    for (i in seq_along(conf.int)) {
      var.obj <- var.beta.logistic(beta0 = x$beta0, beta1 = beta1[i], distribution = x$distribution)

      ncp[i] <- beta1[i] / sqrt(var.obj$var.beta1 / (x$n * (1 - x$parms$r.squared.predictor)))
      sd.ncp[i] <- sqrt((x$vcf * var.obj$var.beta0 + (1 - x$vcf) * var.obj$var.beta1) / var.obj$var.beta1)
    }

    prec.obj <- data.frame(rbind(conf.int, beta1, odds.ratio))

  } else if ("poisson" %in% class(x)) {  # logistic -> poisson

    effect <- c("  Coefficient (Beta)", "  Rate Ratio")

    if (x$parms$method == "signorini") stop("Precision calculations for Signorini method are not supported at the moment.", call. = FALSE)

    var.obj <- var.beta.poisson(beta0 = x$beta0, beta1 = x$beta1, distribution = x$distribution)

    beta1 <- conf.int * sqrt(var.obj$var.beta1 / (x$n * (1 - x$parms$r.squared.predictor) * x$parms$mean.exposure))
    rate.ratio <- exp(beta1)

    ncp <- numeric(length(conf.int))
    sd.ncp <- numeric(length(conf.int))
    for (i in seq_along(conf.int)) {
      var.obj <- var.beta.poisson(beta0 = x$beta0, beta1 = beta1[i], distribution = x$distribution)

      ncp[i] <- beta1[i] / sqrt(var.obj$var.beta1 / (x$n * (1 - x$parms$r.squared.predictor) * x$parms$mean.exposure))
      sd.ncp[i] <- sqrt((x$vcf * var.obj$var.beta0 + (1 - x$vcf) * var.obj$var.beta1) / var.obj$var.beta1)
    }

    prec.obj <- data.frame(rbind(conf.int, beta1, rate.ratio))

  } else if ("mediation" %in% class(x)) {  # poisson -> mediation

    effect <- c("  Indirect Effect", "  Standardized Indirect Effect")

    se.beta.a <- sqrt((1 / x$n) * (x$parms$sd.mediator ^ 2) * (1 - x$r.squared.mediator) / (x$parms$sd.predictor ^ 2))
    se.beta.b <- sqrt((1 / x$n) * (x$parms$sd.outcome ^ 2) * (1 - x$r.squared.outcome) /
                      ((x$parms$sd.mediator ^ 2) * (1 - x$r.squared.mediator)))

    if (x$method == "sobel") {
      se.indirect <- sqrt(x$beta.a ^ 2 * se.beta.b ^ 2  + x$beta.b ^ 2 * se.beta.a ^ 2)
      # lambda <- (x$beta.a * x$beta.b) / se.indirect
    } else if (x$method == "aroian") {
      se.indirect <- sqrt(x$beta.a ^ 2 * se.beta.b ^ 2  + x$beta.b ^ 2 * se.beta.a ^ 2 + se.beta.a ^ 2 * se.beta.b ^ 2)
      # lambda <- (x$beta.a * x$beta.b) / se.indirect
    } else if (x$method == "goodman") {
      se.indirect <- sqrt(x$beta.a ^ 2 * se.beta.b ^ 2  + x$beta.b ^ 2 * se.beta.a ^ 2 - se.beta.a ^ 2 * se.beta.b ^ 2)
      # lambda <- (x$beta.a * x$beta.b) / se.indirect
    }

    beta.indirect <- conf.int * se.indirect
    std.beta.indirect <-  beta.indirect * (x$parms$sd.predictor * x$parms$sd.outcome)

    prec.obj <- data.frame(rbind(conf.int, beta.indirect, std.beta.indirect))

  } # mediation


  # power calculations
  if ("oneprop" %in% class(x)) {

    power <- mapply(FUN = function(mean, null.sd) {
                    tryCatch(power.z.test(mean = mean, sd = 1, null.mean = x$null.mean, null.sd = x$sd,
                                          alternative = x$alternative, verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                    conf.int, null.sd, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  } else if (any(c("logistic", "poisson") %in% class(x))) {

    power <- mapply(FUN = function(mean, sd) {
                    tryCatch(power.z.test(mean = mean, sd = x$sd, null.mean = x$null.mean, null.sd = 1,
                                          alternative = x$alternative, verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                    conf.int, sd.ncp, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  } else {

    power <- vapply(conf.int, function(mean) {
                    tryCatch(power.z.test(mean = mean, sd = 1, null.mean = x$null.mean, null.sd = 1,
                                          alternative = x$alternative, verbose = 0, plot = FALSE)$power,
                             error = function(e) NA_real_) },
                    numeric(1L))

  } # oneprop, logistic / poisson or something else

  prec.obj <- data.frame(rbind(prec.obj, power))
  rownames(prec.obj) <- c("  NCP Uncertainty Distribution", effect, "  Conditional Statistical Power")
  colnames(prec.obj) <- paste0(round(probs * 100, 1), "%")

  invisible(prec.obj)

} # .precision.power.z.test()
# ----------------------------------------------------------------------------------------------------------------------


# helper function(s) ===================================================================================================
.get_rn <- function(class = c()) {
  n_c <- function(name) name %in% class
  if (any(n_c(c("binom", "chisq", "lp")))) {
    c("probs", "quantile", "power")
  } else if (n_c("f")) {
    effect <- if (n_c("generic")) "f.squared" else if (n_c("regression")) "r.squared" else "eta.squared"
    c("probs", "quantile", effect, "power")
  } else if (n_c("t")) {
    effect <- if (n_c("generic")) "t" else if (n_c("regression")) c("beta", "std.beta") else if (n_c("wilcoxon")) c("d", "cles") else "d"
    c("probs", "quantile", effect, "power")
  } else if (n_c("z")) {
    effect <- if (n_c("generic")) "z" else if (n_c("oneprop")) c("prop", "h") else if (n_c("wilcoxon")) c("d", "cles")
                else if (n_c("logistic")) c("beta", "odds.ratio") else if (n_c("poisson")) c("beta", "rate.ratio")
                else if (n_c("mediation")) c("beta.indirect", "std.beta.indirect")
    c("probs", "quantile", effect, "power")
  }
}
