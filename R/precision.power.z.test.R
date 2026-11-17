# asymptotic variances for logistic
# distribution = list(dist = "normal", mean = 0, sd = 1)
# distribution = list(dist = "poisson", lambda = 1)
# distribution = list(dist = "uniform", min = 0, max = 1)
# distribution = list(dist = "exponential", rate = 1)
# distribution = list(dist = "binomial", size = 1, prob = 0.50)
# distribution = list(dist = "bernoulli", prob = 0.50)
# distribution = list(dist = "lognormal", meanlog = 0, sdlog = 1)

# statistic: observed test statistic
# probs: quantile probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.z.test <- function(statistic, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  check.numeric(statistic)
  check.vector(probs, check.proportion)
  
  ci <- tryCatch(stats::qnorm(p = probs, mean = statistic, sd = 1, lower.tail = TRUE),
                 error = function(e) NA_real_)
  
  list(statistic = statistic, probs = probs, ci = setNames(ci, paste0(round(probs * 100, 1), "%")))
  
} # precision.z.test()

precision.z <- precision.z.test


precision.power.z.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = 1, utf = FALSE) {
  
  if (verbose > 0 && length(probs) > 10)
    stop("Printing is not allowed with large output. Try verbose = 0.", call. = FALSE)
  
  if (all(c("pwrss", "z") %in% class(x))) {
    
    utf <- x$parms$utf
    statistic <- x$mean
    null.mean <- x$null.mean
    alternative <- x$alternative

    conf.int <- precision.z.test(
      statistic = statistic,
      probs = probs
    )$ci
    
    if (any(is.na(conf.int))) 
      stop("Calculating confidence interval for power is not possible with the current bounds.",
           call. = FALSE)
    
    if ("generic" %in% class(x)) {
      
      effect <- "z"
      effect.name <- "  z-value"
      utf <- x$utf
      z <- conf.int
      
      precision.power.out <- data.frame(rbind(conf.int, z))
      
    } # generic
    
    if ("wilcoxon" %in% class(x)) {
      
      effect1 <- "d"
      effect2 <- "cles"
      effect1.name <- "  Cohen's d"
      effect2.name <- "  Common Language Effect Size"
      
      method <- x$method
      null.d <- x$parms$null.d
      design <- x$parms$design
      n.ratio <- x$parms$n.ratio
      
      if (design == "independent") {
        n1 <- x$n[1]
        n2 <- x$n[2]
      } else {
        n2 <- x$n
      }
      
      if (method == "noether") {
        
        null.prob <- d.to.cles(d = null.d, design = design, verbose = 0)$cles
        # propss <- n.ratio / (n.ratio + 1)
        # lambda      <- sqrt(n2 + n2 * n.ratio) * sqrt(12 * propss * (1 - propss)) * (prob - null.prob)
        cles <-  null.prob + conf.int / sqrt(12 * n2 * n.ratio / (1 + n.ratio))
        d <- vapply(
          cles,
          FUN = function(clesi) {
            cles.to.d(cles = clesi, design = design, verbose = 0)$d
          },
          FUN.VALUE = numeric(1)
        )
        
      } else if (method == "guenther") {
        
        stop("Guenther methods uses t-test.", call. = FALSE)
        
        if (design == "independent") {
          # lambda      <- (d - null.d) / sqrt(1 / n1 + 1 / n2)
          d <- conf.int * sqrt(1 / n1 + 1 / n2) + null.d
        } else {
          # lambda      <- (d - null.d) / sqrt(1 / n2)
          d <- conf.int * sqrt(1 / n2) + null.d
        }
        
        cles <- vapply(
          d,
          FUN = function(di) {
            d.to.cles(d = di, design = design, verbose = 0)$cles
          },
          FUN.VALUE = numeric(1)
        )
        
      } # guenther or noether 
      
      precision.power.out <- data.frame(rbind(conf.int, d, cles))
      
    } # wilcoxon
    
    
    if ("logistic" %in% class(x)) {
      
      effect1 <- "beta"
      effect2 <- "odds.ratio"
      effect1.name <- "  Coefficient (Beta)"
      effect2.name <- "  Odds Ratio"
      
      method <- tolower(x$parms$method)
      if (method == "hsieh") stop("Precision calculations for Hsieh method are not supported at the moment.", call. = FALSE)
    
      r.squared.predictor <- x$parms$r.squared.predictor
      distribution <- x$distribution
      beta0 <- x$beta0
      beta1 <- x$beta1
      vcf <- x$vcf
      n <- x$n
      var.obj <- var.beta.logistic(beta0 = beta0, beta1 = beta1, distribution = distribution)
      var.beta0 <- var.obj$var.beta0
      var.beta1 <- var.obj$var.beta1
  
      # conf.int <- qnorm(probs, mean = 0, sd = 1, lower.tail = TRUE)
      # beta1 <- beta1 + conf.int * sqrt(var.beta1 / (n * (1 - r.squared.predictor)))
      beta1 <- conf.int * sqrt(var.beta1 / (n * (1 - r.squared.predictor)))
      odds.ratio <- exp(beta1)
        
      ncp <- numeric(length(conf.int))
      sd.ncp <- numeric(length(conf.int))
      for(i in 1:length(conf.int)) {
        var.obj <- var.beta.logistic(beta0 = beta0, beta1 = beta1[i], distribution = distribution)
        var.beta0 <- var.obj$var.beta0
        var.beta1 <- var.obj$var.beta1
        ncp[i] <- beta1[i] / sqrt(var.beta1 / (n * (1 - r.squared.predictor)))
        sd.ncp[i] <- sqrt((vcf * var.beta0 + (1 - vcf) * var.beta1) / var.beta1)
        }
        
      precision.power.out <- data.frame(rbind(conf.int, beta1, odds.ratio))
      
    } # logistic
      
        
    if ("poisson" %in% class(x)) {
      
      effect1 <- "beta"
      effect2 <- "rate.ratio"
      effect1.name <- "  Coefficient (Beta)"
      effect2.name <- "  Rate Ratio"
      
      method <- tolower(x$parms$method)
      if (method == "signorini") stop("Precision calculations for Signorini method are not supported at the moment.", call. = FALSE)
      
      r.squared.predictor <- x$parms$r.squared.predictor
      mean.exposure <- x$parms$mean.exposure
      distribution <- x$distribution
      beta0 <- x$beta0
      beta1 <- x$beta1
      vcf <- x$vcf
      n <- x$n
      var.obj <- var.beta.poisson(beta0 = beta0, beta1 = beta1, distribution = distribution)
      var.beta0 <- var.obj$var.beta0
      var.beta1 <- var.obj$var.beta1
      
      # conf.int <- qnorm(probs, mean = 0, sd = 1, lower.tail = TRUE)
      # beta1 <- beta1 + conf.int * sqrt(var.beta1 / (n * (1 - r.squared.predictor) * mean.exposure))
      beta1 <- conf.int * sqrt(var.beta1 / (n * (1 - r.squared.predictor) * mean.exposure))
      rate.ratio <- exp(beta1)
      
      ncp <- numeric(length(conf.int))
      sd.ncp <- numeric(length(conf.int))
      for(i in 1:length(conf.int)) {
        var.obj <- var.beta.poisson(beta0 = beta0, beta1 = beta1[i], distribution = distribution)
        var.beta0 <- var.obj$var.beta0
        var.beta1 <- var.obj$var.beta1
        ncp[i] <- beta1[i] / sqrt(var.beta1 / (n * (1 - r.squared.predictor) * mean.exposure))
        sd.ncp[i] <- sqrt((vcf * var.beta0 + (1 - vcf) * var.beta1) / var.beta1)
      }
      
      precision.power.out <- data.frame(rbind(conf.int, beta1, rate.ratio))
      
    } # poisson
    
    
    if ("mediation" %in% class(x)) {
      
      effect1 <- "beta.indirect"
      effect2 <- "std.beta.indirect"
      effect1.name <- "  Indirect Effect"
      effect2.name <- "  Standardized Indirect Effect"
      
      n <- x$n
      beta.a <- x$beta.a
      beta.b <- x$beta.b
      method <- x$method
      r.squared.mediator <- x$r.squared.mediator
      r.squared.outcome <- x$r.squared.outcome
      sd.mediator <- x$parms$sd.mediator
      sd.predictor <- x$parms$sd.predictor
      sd.outcome <- x$parms$sd.outcome
      
      se.beta.a <- sqrt((1 / n) * (sd.mediator ^ 2) * (1 - r.squared.mediator) / (sd.predictor ^ 2))
      se.beta.b <- sqrt((1 / n) * (sd.outcome ^ 2) * (1 - r.squared.outcome) / ((sd.mediator ^ 2) * (1 - r.squared.mediator)))
      
      if (method == "sobel") {
        se.indirect <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2)
        # lambda <- (beta.a * beta.b) / se.indirect
      } else if (method == "aroian") {
        se.indirect<- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 + se.beta.a ^ 2 * se.beta.b ^ 2)
        # lambda <- (beta.a * beta.b) / se.indirect
      } else if (method == "goodman") {
        se.indirect <- sqrt(beta.a ^ 2 * se.beta.b ^ 2  + beta.b ^ 2 * se.beta.a ^ 2 - se.beta.a ^ 2 * se.beta.b ^ 2)
        # lambda <- (beta.a * beta.b) / se.indirect
      }
      
      beta.indirect <- conf.int * se.indirect
      std.beta.indirect <-  beta.indirect * (sd.predictor * sd.outcome)
      
      precision.power.out <- data.frame(rbind(conf.int, beta.indirect, std.beta.indirect))
      
    } # mediation
    
    if ("oneprop" %in% class(x)) {
      
      z.to.prob <- function(z, null.prob, n, std.error, arcsine, correct, alternative) {
        
        solve.one.prob.noarcsine <- function(null.prob, z, k, n, alternative) {
          
          A <- null.prob - k
          B <- (z^2) / n
          discriminant <- B * (B + 4 * A * (1 - A))
          if (discriminant < 0) return(NA) 
          sqrt.disc <- sqrt(discriminant)
          p1 <- (2 * A + B + sqrt.disc) / (2 * (1 + B))
          p2 <- (2 * A + B - sqrt.disc) / (2 * (1 + B))
          candidates <- c(p1, p2)
          if (z >= 0) {
            final.prob <- candidates[candidates > null.prob]
        } else {
            final.prob <- candidates[candidates < null.prob]
        }
          final.prob <- final.prob[final.prob >= 0 & final.prob <= 1]
          if (length(final.prob) == 0) return(NA)
        
          return(final.prob[1])
        
        } # slove.one.prob
        
        if (arcsine) {
          
          h <- z * sqrt(1 / n)
          
          prob <- switch(alternative,
                         `two.sided` =  h.to.probs(h = h,
                                                   prob1 = NULL,
                                                   prob2 = null.prob,
                                                   verbose = FALSE)$prob1,
                         `one.sided` =  h.to.probs(h = h, 
                                                   prob1 = NULL,
                                                   prob2 = null.prob, 
                                                   verbose = FALSE)$prob1,
                         `two.one.sided` = h.to.probs(h = h, 
                                                      prob1 = NULL,
                                                      prob2 = null.prob, 
                                                      verbose = FALSE)$prob1)
          
        } else {
          
          if (correct) {
            k <- switch(alternative,
                        `one.sided` = sign(z) / (2 * n),
                        `two.sided` = -1 / (2 * n),
                        `two.one.sided` =  sign(z) * 1 / (2 * n))
          } else {
            k <- 0
        }
        
          prob <- switch(alternative,
                         `two.sided` = solve.one.prob.noarcsine(null.prob = null.prob,
                                                                z = z,
                                                                k = k, 
                                                                n = n),
                         `one.sided` = solve.one.prob.noarcsine(null.prob = null.prob,
                                                                z = z,
                                                                k = k,
                                                                n = n),
                         `two.one.sided` = solve.one.prob.noarcsine(null.prob = null.prob, 
                                                                    z = z, 
                                                                    k = k,
                                                                    n = n))
        } # if arcsine
        
        null.sd <- ifelse(std.error == "null" && alternative %in% c("two.sided", "one.sided"),
                          sqrt((null.prob * (1 - null.prob)) / (prob * (1 - prob))), 1)
      
        list(prob = prob, null.sd = null.sd)
        
      } # z.to.prob
      
      effect1 <- "prop"
      effect2 <- "h"
      effect1.name <- "  Proportion"
      effect2.name <- "  Cohen's h"
        
      n <- x$n
      null.prob <- x$null.prob
      null.mean <- x$null.mean
      std.error <- x$parms$std.error
      arcsine <- x$parms$arcsine
      correct <- x$parms$correct
      alternative <- x$alternative
      std.error <- x$std.error
      
      props <- numeric(length(conf.int))
      null.sd <- numeric(length(conf.int))
      
      for(i in 1:length(conf.int)) {
        
        if (alternative == "two.one.sided") {
          
          ######################################################################
          if (all(sign(null.mean) == -1)) {
            z <- conf.int[i] + abs(max(null.mean))
            null.prob <- max(null.prob)
          } else if (all(sign(null.mean) == 1)) {
            z <- conf.int[i] - min(null.mean)
            null.prob <- min(null.prob)
          } else {
            z <- conf.int[i] - null.mean[sign(null.mean) == 1]
            null.prob <- max(null.prob)
            
            print(z) ###########################################################
            print(null.prob) ###################################################
          }
          ######################################################################
          
        } else {
          
          z <- conf.int[i]
          
        } # two.one.sided
          
        zp.obj <- z.to.prob(z = z, 
                            null.prob = null.prob,
                            n = n, 
                            std.error = std.error,
                            arcsine = arcsine, 
                            correct = correct,
                            alternative = alternative)
        
        props[i] <- zp.obj$prob
        null.sd[i] <- zp.obj$null.sd
        h[i] <- probs.to.h(prob1 = zp.obj$prob, prob2 = null.prob, verbose = 0)$h
        
      } # for loop
      
    
      precision.power.out <- data.frame(rbind(conf.int, props, h))
      
    } # oneprop
  
  
  
  
  
    # power calculations
    
    if (any(c("logistic", "poisson") %in% class(x))) {
      
      power <- mapply(FUN =      function(x, y) {
        tryCatch({
          power.z.test(
            mean = x, 
            sd = y,
            null.mean = null.mean,
            null.sd = 1,
            alternative = alternative,
            verbose = 0,
            plot = FALSE
          )$power
        }, error = function(e) {
          NA_real_
        })
      },
      conf.int,
      sd.ncp,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
      )
      
    } else if ("oneprop" %in% class(x)) {
        
        power <- mapply(FUN = function(x, y) {
          tryCatch({
            power.z.test(
              mean = x, 
              sd = 1,
              null.mean = null.mean,
              null.sd = y,
              alternative = alternative,
              verbose = 0,
              plot = FALSE
            )$power
          }, error = function(e) {
            NA_real_
          })
        },
        conf.int,
        null.sd,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
      )
      
    } else {
      
      power <- vapply(
        conf.int,
        function(x) {
          tryCatch({
            power.z.test(
              mean = x, 
              sd = 1,
              null.mean = null.mean,
              null.sd = 1,
              alternative = alternative,
              verbose = 0,
              plot = FALSE
            )$power
          }, error = function(e) {
            NA_real_
          })
        },
        numeric(1L)
      )
      
    } # logistic or poisson?
    
    precision.power.out <- data.frame(rbind(precision.power.out, power))
    colnames(precision.power.out) <-  paste0(round(probs * 100, 1), "%")

    if (any(c("logistic", "poisson", "wilcoxon", "mediation", "oneprop") %in% class(x))) { 
      rownames(precision.power.out) <- c("  NCP Uncertainty Distribution", 
                                  effect1.name, effect2.name,
                                  "  Conditional Statistical Power")
    } else {
      rownames(precision.power.out) <- c("  NCP Uncertainty Distribution", 
                                  effect.name,
                                  "  Conditional Statistical Power")
    }
    
    if (verbose > 0) {
      if (utf) {
        cat(paste0(paste0("\u2554", strrep("\u2550", 50), "\u2557", "\n"),
                   paste0("\u2551", 
                          "             \033[34m PRECISION CALCULATION \033[0m              ",
                          "\u2551", "\n"),
                   paste0("\u255A", strrep("\u2550", 50), "\u255D", "\n\n")))
      } else {
        cat("+--------------------------------------------------+ \n",
            "|              PRECISION CALCULATION               | \n",
            "+--------------------------------------------------+ \n\n", 
            sep = "")
      }
      print(round(precision.power.out, 3))
      if (verbose > 1)
        if (utf) {
          cat(paste0("\033[36m",
                     "\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  directly from the cumulative distribution function of \n",
                     "  non-central z-distribution given the observed z-stat.\n",
                     "\033[0m"))
        } else {
          cat(paste0("\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  directly from the cumulative distribution function of \n",
                     "  non-central z-distribution given the observed z-stat.\n"))
        }

    } # verbose
  
    precision.power.out <- data.frame(rbind(probs, precision.power.out))
   
    if (any(c("logistic", "poisson", "wilcoxon", "mediation", "oneprop") %in% class(x))) { 
      rownames(precision.power.out) <- c("probs", "quantile", effect1, effect2, "power")
    } else {
      rownames(precision.power.out) <- c("probs", "quantile", effect, "power")
    }
    colnames(precision.power.out) <- NULL
    
    return(invisible(as.data.frame(t(precision.power.out))))
    
  } else {
    
    stop("Object should be a class of 'pwrss' and 'z'", call. = FALSE)
  }
  
} # precision.power.z.test()

precision.power.z <- precision.power.z.test
