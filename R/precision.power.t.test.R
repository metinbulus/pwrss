# statistic: observed t-statistic
# probs: inversion probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.t.test <- function(statistic, df, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  
  if (!is.numeric(statistic) ||
      length(statistic) != 1L ||
      !is.finite(statistic)) {
    stop("'statistic' must be one finite number.",
         call. = FALSE)
  }
  
  if (!is.numeric(df) ||
      length(df) != 1L ||
      !is.finite(df) ||
      df <= 0) {
    stop("'df' must be one positive finite number.",
         call. = FALSE)
  }
  
  if (!is.numeric(probs) ||
      any(!is.finite(probs)) ||
      any(is.na(probs)) ||
      any(probs < 0.000001) ||
      any(probs > 0.999999)) {
    stop("'probs' must be between 0.000001 and 0.999999.",
         call. = FALSE)
  }
  
  cdf <- function(ncp) {
    tryCatch(
      suppressWarnings(
        stats::pt(
          statistic,
          df = df,
          ncp = ncp
        )
      ),
      error = function(e) NA_real_
    )
  }
  
  find.ncp.t <- function(target) {
    
    # Function whose root we want:
    # pt(...) = target
    objective <- function(ncp) {
      value <- cdf(ncp)
      
      if (!is.finite(value)) {
        return(NA_real_)
      }
      
      value - target
    }
    
    # The noncentrality parameter may be negative or positive.
    # Start searching around the observed t statistic.
    width <- 1
    bracketed <- FALSE
    
    for (i in seq_len(500L)) {
      
      lower <- statistic - width
      upper <- statistic + width
      
      value.at.lower <- objective(lower)
      value.at.upper <- objective(upper)
      
      # The CDF decreases as ncp increases.
      #
      # A root is bracketed when:
      # objective(lower) >= 0
      # objective(upper) <= 0
      if (is.finite(value.at.lower) &&
          is.finite(value.at.upper) &&
          value.at.lower >= 0 &&
          value.at.upper <= 0) {
        bracketed <- TRUE
        break
      }
      
      width <- width * 2
      
      if (!is.finite(width)) {
        break
      }
    }
    
    if (!bracketed) {
      return(NA_real_)
    }
    
    tryCatch(
      stats::uniroot(
        objective,
        interval = c(lower, upper),
        tol = .Machine$double.eps^0.25,
        maxiter = 500L
      )$root,
      error = function(e) NA_real_
    )
    
  } # find.ncp.t
  
  ci <- vapply(
    1 - probs,
    function(target) {
      find.ncp.t(
        target = target
      )
    },
    numeric(1L)
  )
  
  names(ci) <- paste0(round(probs * 100, 1), "%")
  
  if (any(abs(ci) > 35, na.rm = TRUE)) {
    warning(
      paste(
        "At least one confidence limit exceeds 35 in magnitude;",
        "base R noncentral-t probabilities may be inaccurate."
      ),
      call. = FALSE
    )
  }
  
  list(
    statistic = statistic,
    df = df,
    probs = probs,
    ci = ci
  )
  
} # precision.t.test()

precision.t <- precision.t.test

library(MBESS)

ours <- precision.t.test(
  statistic = 2.83,
  df = 126
)$ci

mbess <- MBESS::conf.limits.nct(
  ncp = 2.83,
  df = 126,
  conf.level = 0.95
)

mbess.ci <- c(
  lower = mbess$Lower.Limit,
  upper = mbess$Upper.Limit
)

rbind(
  precision.t.test = ours,
  MBESS = mbess.ci
)

all.equal(
  ours,
  mbess.ci,
  tolerance = 1e-6,
  check.attributes = FALSE
)


precision.power.t.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = TRUE) {
  
  if(verbose > 0 && length(probs) > 10)
    stop("Printing is not allowed with large output. Try verbose = FALSE.",
         call. = FALSE)
  
  if (all(c("pwrss", "t") %in% class(x))) {
    
    utf <- x$parms$utf
    statistic <- x$ncp
    null.ncp <- x$null.ncp
    alternative <- x$alternative
    df <- x$df
    
    conf.int <- precision.t.test(
      statistic = statistic,
      df = df,
      probs = probs
    )$ci
    
    power <- vapply(
      conf.int,
      function(x) {
        tryCatch({
          power.t.test(
            ncp = x,
            null.ncp = null.ncp,
            alternative = alternative,
            df = df,
            verbose = 0,
            plot = FALSE
          )$power
        }, error = function(e) {
          NA_real_
        })
      },
      numeric(1L)
    )
    
    if(any(is.na(conf.int))) 
      stop("Calculating confidence interval for power is not possible with the current bounds.",
           call. = FALSE)
    
    if("generic" %in% class(x)) {
      
      effect <- "t"
      effect.name <- "  t-value"
      utf <- x$utf
      t <- conf.int

      precision.power.out <- data.frame(rbind(conf.int, t, power))
      
    }
    
    if("welch" %in% class(x)) {
      
      effect <- "d"
      effect.name <- "  Cohen's d"
      
      null.d <- x$parms$null.d
      n.ratio <- x$parms$n.ratio
      var.ratio <- x$parms$var.ratio
      claim.basis <- x$parms$claim.basis
      n1 <- x$n[1]
      n2 <- x$n[2]
      
      # variance ratio constraint
      sd2  <- sqrt((n2 * (n.ratio + 1) - 2) / (n2 - 1 + var.ratio * (n2 * n.ratio - 1)))
      sd1 <- sqrt(var.ratio * sd2 ^ 2)
      var.delta <- 1 / n1 * sd1 ^ 2 + 1 / n2 * sd2 ^ 2
      std <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
      var.std <- ((n1 - 1) * sd1 ^ 4 + (n2 - 1) * sd2 ^ 4) / ((2 * (n1 - 1) * sd1 ^ 2 + 2 * (n2 - 1) * sd2 ^ 2) * (n1 + n2 - 2))
      
      if (claim.basis == "md.pval") {
        d <- conf.int * sqrt(var.delta) + null.d
      } else {
        disc <- (-2 * null.d)^2 - 4 * (1 - conf.int^2 * (var.std   / std^2)) * (null.d^2 - conf.int^2 * (var.delta / std^2))
        if (any(disc < 0)) {
          warning("No real solution for some d.", call. = FALSE)
          disc[disc < 0] <- NA_real_
        }
        d1 <- (2 * null.d + sqrt(disc)) / (2 * (1 - conf.int^2 * (var.std   / std^2)))
        d2 <- (2 * null.d - sqrt(disc)) / (2 * (1 - conf.int^2 * (var.std   / std^2)))
        d <- ifelse(sign(d1) == sign(conf.int), d1, d2)
      }
      
      precision.power.out <- data.frame(rbind(conf.int, d, power))
      
    } # welch
    
    
    if("student" %in% class(x)) {
      
      effect <- "d"
      effect.name <- "  Cohen's d"
      
      null.d <- x$parms$null.d
      n.ratio <- x$parms$n.ratio
      design <- x$parms$design
      claim.basis <- x$parms$claim.basis

      if (design == "independent") {
        
        n1 <- x$n[1]
        n2 <- x$n[2]
        if (claim.basis == "md.pval") {
          var.delta <- (n1 + n2) / (n1 * n2) 
          d <- conf.int * sqrt(var.delta) + null.d
        } else {
          # var.delta <- (n1 + n2) / (n1 * n2) + d ^ 2 / (2 * df)
          # d <- conf.int * sqrt(var.delta) + null.d
          disc <- (-2 * null.d)^2 - 4 * (1 - (conf.int^2) * (1 / (2 * df))) * (null.d^2 - (conf.int^2) * ((n1 + n2) / (n1 * n2)))
          if (any(disc < 0)) {
            warning("No real solution for some d.", call. = FALSE)
            disc[disc < 0] <- NA_real_
          }
          d1 <- (2 * null.d + sqrt(disc)) / (2 * (1 - (conf.int^2) * (1 / (2 * df))))
          d2 <- (2 * null.d - sqrt(disc)) / (2 * (1 - (conf.int^2) * (1 / (2 * df))))
          d <- ifelse(sign(d1) == sign(conf.int), d1, d2)
        }
        
      } else {
        
        n2 <- x$n
        if (claim.basis == "md.pval") {
          var.delta <- 1 / n2
          d <- conf.int * sqrt(var.delta) + null.d
        } else {
          # var.delta <- 1 / n2 + d ^ 2 / (2 * n2)
          # d <- conf.int * sqrt(var.delta) + null.d
          disc <- (-2 * null.d)^2 - 4 * (1 - (conf.int^2) * (1 / (2 * n2))) * (null.d^2 - (conf.int^2) * (1 / n2))
          if (any(disc < 0)) {
            warning("No real solution for some d.", call. = FALSE)
            disc[disc < 0] <- NA_real_
          }
          d1 <- (2 * null.d  + sqrt(disc)) / (2 * (1 - (conf.int^2) * (1 / (2 * n2))))
          d2 <- (2 * null.d  - sqrt(disc)) / (2 * (1 - (conf.int^2) * (1 / (2 * n2))))
          d <- ifelse(sign(d1) == sign(conf.int), d1, d2)
        }
        
      } # independent?
      
      precision.power.out <- data.frame(rbind(conf.int, d, power))
      
    } # student
    
      
    if("regression" %in% class(x)) {
      
      effect1 <- "beta"
      effect2 <- "std.beta"
      effect1.name <- "  Coefficient (Beta)"
      effect2.name <- "  Standardized Beta"
      
      parms.r.squared <- x$parms$r.squared
      if(is.null(parms.r.squared)) stop("R-squared cannot be NULL with precision calculations.", call. = FALSE)
      sd.outcome <- x$parms$sd.outcome
      sd.predictor <- x$parms$sd.predictor
      r.squared <- x$r.squared
      null.beta <- x$parms$null.beta
      k.total <- x$parms$k.total
      n <- x$n

      # lambda <- (beta - null.beta)  / ((sd.outcome / sd.predictor) * sqrt((1 - r.squared) / n))
      beta <- conf.int * ((sd.outcome / sd.predictor) * sqrt((1 - r.squared) / n)) + null.beta
      std.beta <- beta * (sd.predictor / sd.outcome)
      precision.power.out <- data.frame(rbind(conf.int, beta, std.beta, power))
      
    } # regression
    
    
    if("wilcoxon" %in% class(x)) {
      
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
        
        stop("Noether methods uses z-test.", call. = FALSE)
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
      
      precision.power.out <- data.frame(rbind(conf.int, d, cles, power))
      
    } # wilcoxon
    
    
    colnames(precision.power.out) <-  paste0(round(probs * 100, 1), "%")
    if(any(c("wilcoxon", "regression") %in% class(x))) { 
      rownames(precision.power.out) <- c("  NCP Uncertainty Distribution", 
                                  effect1.name, effect2.name,
                                  "  Conditional Statistical Power")
    } else {
      rownames(precision.power.out) <- c("  NCP Uncertainty Distribution", 
                                  effect.name,
                                  "  Conditional Statistical Power")
    }
    
    if(verbose > 0) {
      if(utf) {
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
      print(precision.power.out, digits = 3)
      if(verbose > 1)
        if(utf) {
          cat(paste0("\033[36m",
                     "\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  by inverting cumulative distribution function of the \n",
                     "  non-central t-distribution given the observed t-stat.\n",
                     "\033[0m"))
        } else {
          cat(paste0("\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  by inverting cumulative distribution function of the \n",
                     "  non-central t-distribution given the observed t-stat.\n"))
        }

    } # verbose
    
    precision.power.out <- rbind(probs, precision.power.out)
    if(any(c("wilcoxon", "regression") %in% class(x))) { 
      rownames(precision.power.out) <- c("probs", "quantile", effect1, effect2, "power")
    } else {
      rownames(precision.power.out) <- c("probs", "quantile", effect, "power")
    }
    
    colnames(precision.power.out) <- NULL
    
    return(invisible(as.data.frame(t(precision.power.out))))
    
  } else {
    
    stop(
      "Object should be a class of 'pwrss' and 't'",
      call. = FALSE
    )
  }
  
} # precision.power.t.test()

precision.power.t <- precision.power.t.test



################################################################################
############################# validations ######################################
################################################################################

############################# generic

power.t.test(ncp = NULL, 
             null.ncp = 0,
             df = 30,
             power = 0.50,
             utf = TRUE) |>
  precision.power.t.test(verbose = 1)

power.t.test(ncp = NULL, 
             null.ncp = c(-2, 2),
             df = 30,
             power = 0.50,
             alternative = "two.one.sided",
             utf = FALSE) |>
  precision.power.t.test(verbose = 1)
############################# welch

obj <- power.t.welch(d = -0.3536124, 
              var.ratio = 4,
              n.ratio = 0.50,
              n2 = 1000,
              ceil.n = FALSE,
              claim.basis = "smd.ci",
              # claim.basis = "md.pval",
              utf = TRUE) |>
  precision.power.t.test(verbose = 1)

delta.obj <- hc.t.test(m1 = 0, m2 = 0.50, sd1 = 2, sd2 = 1, n1 = 500, n2 = 1000,
                       method = "hc2", df = "welch-satterthwaite")
d.obj <- smd.hc.t.test(delta.obj)
d.obj$es

############################# student

power.t.student(d = 0.3536124, 
              n.ratio = 0.50,
              n2 = 1000,
              ceil.n = FALSE,
              # claim.basis = "smd.ci",
              claim.basis = "md.pval",
              design = "one.sample",
              utf = TRUE) |>
  precision.power.t.test(verbose = 1)

delta.obj <- hc.t.test(m1 = 0.50, m2 = 0, sd1 = 2, sd2 = 1, n1 = 500, n2 = 1000, method = "const")
d.obj <- smd.hc.t.test(delta.obj)
d.obj$es


############################# paired and one sample

mock_data <- data.frame(
  student_id = 1:10,
  pre_test   = c(55, 62, 60, 58, 70, 65, 52, 68, 61, 59),
  post_test  = c(65, 70, 68, 62, 78, 72, 58, 80, 71, 66)
)

effectsize::cohens_d(x = mock_data$pre_test, y = mock_data$post_test, paired = TRUE)

power.t.student(d = -3.54, 
                n2 = 10,
                ceil.n = FALSE,
                # claim.basis = "smd.ci",
                claim.basis = "md.pval",
                design = "paired",
                utf = TRUE) |>
  precision.power.t.test(verbose = 1)

############################# wilcoxon

power.t.student(d = 0.50, 
                n.ratio = 1,
                n2 = 64,
                ceil.n = FALSE,
                # claim.basis = "smd.ci",
                claim.basis = "md.pval",
                design = "independent",
                verbose = 0,
                utf = TRUE) |>
  precision.power.t.test(verbose = 1)

power.np.wilcoxon(d = 0.50, 
                n.ratio = 1,
                n2 = 64,
                ceil.n = FALSE,
                design = "independent",
                method = "guenther", # "guenther", "noether"
                verbose = 0,
                utf = TRUE) |>
  precision.power.t.test(verbose = 1)

hc.t.test(m1 = 0.50, m2 = 0,
          sd1 = 1, sd2 = 1,
          n1 = 64, n2 = 64,
          method = "const") |> 
  smd.hc.t.test()



############################# regression

power.t.reg(beta = 0.50, sd.predictor = 0.50,
            power = 0.80,
            r.squared = 0,
            verbose = 2,
            utf = TRUE) |>
  precision.power.t.test(verbose = 2)


set.seed(123)
n <- 1000000
predictor <- rnorm(n, mean = 50, sd = 10)
covariate <- rnorm(n, mean = 100, sd = 15)
outcome   <- 5 + (0.4 * predictor) + (0.2 * covariate) + rnorm(n, mean = 0, sd = 5)

mock_data <- data.frame(outcome, predictor, covariate)

sd_outcome   <- sd(mock_data$outcome)
sd_predictor <- sd(mock_data$predictor)

model_raw <- lm(outcome ~ predictor + covariate, data = mock_data)

adjusted_r2 <- summary_raw$adj.r.squared
r2 <- summary_raw$r.squared

mock_data_std <- as.data.frame(scale(mock_data))
model_std_manual <- lm(outcome ~ predictor + covariate, data = mock_data_std)


power.t.reg(beta = coef(model_std_manual)[[2]], 
            r.squared = adjusted_r2,
            k.total = 2,
            n = n,
            utf = TRUE) |>
  precision.power.t.test(verbose = 1)

confint(model_std_manual)

power.t.reg(beta = coef(model_raw)[[2]], 
            sd.outcome = sd_outcome,
            sd.predictor = sd_predictor,
            r.squared = adjusted_r2, 
            # r.squared = r2, 
            k.total = 2,
            n = n,
            utf = TRUE) |>
  precision.power.t.test(verbose = 1)

confint(model_raw)



