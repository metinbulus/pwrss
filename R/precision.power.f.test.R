# statistic: observed F-statistic
# probs: inversion probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.f.test <- function(statistic, df1, df2, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  
  if (!is.numeric(statistic) ||
      length(statistic) != 1L ||
      !is.finite(statistic) ||
      statistic < 0) {
    stop("'statistic' must be one nonnegative finite number.",
         call. = FALSE)
  }
  
  if (!is.numeric(df1) ||
      length(df1) != 1L ||
      !is.finite(df1) ||
      df1 <= 0) {
    stop("'df1' must be one positive finite number.",
         call. = FALSE)
  }
  
  if (!is.numeric(df2) ||
      length(df2) != 1L ||
      !is.finite(df2) ||
      df2 <= 0) {
    stop("'df2' must be one positive finite number.",
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
        stats::pf(
          statistic,
          df1 = df1,
          df2 = df2,
          ncp = ncp
        )
      ),
      error = function(e) NA_real_
    )
  }
  
  find.ncp.f <- function(target, no.root = 0) {
    
    # Function whose root we want:
    # pf(...) = target
    objective <- function(ncp) {
      suppressWarnings(
        stats::pf(
          statistic,
          df1 = df1,
          df2 = df2,
          ncp = ncp
        )
      ) - target
    }
    
    # Evaluate the objective at the smallest possible ncp.
    value.at.zero <- objective(0)
    
    if (!is.finite(value.at.zero)) {
      return(NA_real_)
    }
    
    # The CDF decreases as ncp increases.
    #
    # If the CDF is already below the target at ncp = 0,
    # no nonnegative root exists.
    if (value.at.zero < 0) {
      return(no.root)
    }
    
    # Start searching with an upper boundary of 1.
    upper <- 1
    
    # Increase the upper boundary until the objective
    # changes from positive to negative.
    for (i in seq_len(500L)) {
      
      value.at.upper <- objective(upper)
      
      if (is.finite(value.at.upper) &&
          value.at.upper <= 0) {
        break
      }
      
      upper <- upper * 2
    }
    
    # If no valid upper boundary was found, return NA.
    if (!exists("value.at.upper") ||
        !is.finite(value.at.upper) ||
        value.at.upper > 0) {
      return(NA_real_)
    }
    
    tryCatch(
      stats::uniroot(
        objective,
        interval = c(0, upper),
        tol = .Machine$double.eps^0.25,
        maxiter = 500L
      )$root,
      error = function(e) NA_real_
    )
    
  } # find.ncp.f
  
  ci <- vapply(
    1 - probs,
    function(target) {
      find.ncp.f(
        target = target
      )
    },
    numeric(1L)
  )
  
  names(ci) <- paste0(round(probs * 100, 1), "%")
  
  list(
    statistic = statistic,
    df1 = df1,
    df2 = df2,
    probs = probs,
    ci = ci
  )
  
} # precision.f.test()

precision.f <- precision.f.test

library(MBESS)

precision.f.test(
  statistic = 11.221,
  df1 = 4,
  df2 = 50
)$ci

mbess <- MBESS::conf.limits.ncf(
  F.value = 11.221,
  df.1 = 4,
  df.2 = 50,
  conf.level = 0.95
)

c(
  lower = mbess$Lower.Limit,
  upper = mbess$Upper.Limit
)


precision.power.f.test <- function(x,
                            probs = c(0.025, 0.50, 0.975),
                            verbose = TRUE) {
  
  if(verbose > 0 && length(probs) > 10)
    stop("Printing is not allowed with large output. Try verbose = FALSE.",
         call. = FALSE)
  
  if (all(c("pwrss", "f") %in% class(x))) {
    
    if("generic" %in% class(x)) {
      effect <- "f.squared"
      effect.name <- "  Cohen's f-squared"
      utf <- x$utf
      ncp <- x$ncp
      null.ncp <- x$null.ncp
      df1 <- x$df1
      df2 <- x$df2
      n.total <- df1 + df2 + 1
      epsilon <- 1
      f.squared <- ncp / (n.total * epsilon)
      f.statistic <- f.squared * (df2 / df1)
    }
    
    if("anova_mixed" %in% class(x)) {
      # stop("Precision calculations does not currently allow repeated measures ANOVA model.",
      #     call. = FALSE)
      # lambda <- f.squared * n.total * epsilon
       effect <- "eta.squared"
       effect.name <- "  (Partial) Eta-squared"
       utf <- x$parms$utf
       df1 <- x$df1 # epsilon adjusted
       df2 <- x$df2 # epsilon adjusted
       n.total <- x$n.total
       eta.squared <- x$eta.squared
       ncp <- x$ncp
       null.ncp <- x$null.ncp
       epsilon <- x$parms$epsilon
       f.squared <- ncp / (n.total * epsilon)
       # f.squared <- eta.squared / (1 - eta.squared)
       f.statistic <- f.squared * (df2 / df1)
    }
    
    if("regression" %in% class(x)) {
      # lambda <- f.squared * n.total 
      effect <- "r.squared"
      effect.name <- "  (Delta) R-squared"
      utf <- x$parms$utf
      df1 <- x$df1
      df2 <- x$df2
      n.total <- x$n
      ncp <- x$ncp
      null.ncp <- x$null.ncp
      epsilon <- 1 # for consistency
      f.squared <- ncp / n.total
      f.statistic <- f.squared * (df2 / df1)
    }
    
    if("ancova" %in% class(x)) {
      # lambda <- f.squared * n.total 
      effect <- "eta.squared"
      effect.name <- "  Eta-squared"
      utf <- x$parms$utf
      df1 <- x$df1
      df2 <- x$df2
      epsilon <- 1 # for consistency
      ncp <- x$ncp
      null.ncp <- x$null.ncp
      n.total <- x$n.total
      f.squared <- ncp / n.total
      f.statistic <- f.squared * (df2 / df1)
    }
    
    conf.int <- precision.f.test(
      statistic = f.statistic,
      df1 = df1,
      df2 = df2,
      probs = probs
    )$ci
    
    # f.squared.ci <- conf.int / (n.total * epsilon)
    f.squared.ci <- conf.int / df2
    r.squared.ci <- f.squared.ci / (1 + f.squared.ci)

    if(any(is.na(conf.int)) || all(conf.int == 0)) 
      stop("Calculating confidence interval for power is not possible with the current bounds.", 
           call. = FALSE)
    
    conf.int[conf.int == 0] <- 1e-10
    
    power <- vapply(
      conf.int,
      function(x) {
        tryCatch({
          power.f.test(ncp = x, 
                       null.ncp = null.ncp,
                       df1 = df1,
                       df2 = df2,
                       verbose = 0, 
                       plot = FALSE)$power
        }, error = function(e) {
          NA_real_
        })
      },
      numeric(1L)
    )
    
    if(effect == "f.squared") r.squared.ci <- f.squared.ci
    precision.power.out <- data.frame(rbind(conf.int, r.squared.ci, power))
    colnames(precision.power.out) <-  paste0(round(probs * 100, 1), "%")
    rownames(precision.power.out) <- c("  NCP Uncertainty Distribution", 
                                effect.name ,
                                "  Conditional Statistical Power")
    
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
      print(round(precision.power.out, 3))
      if(verbose > 1)
        if(utf) {
          cat(paste0("\033[36m",
                     "\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  by inverting cumulative distribution function of the \n",
                     "  non-central F-distribution given the observed F-stat.\n",
                     "\033[0m"))
        } else {
          cat(paste0("\n  NCP uncertainty distribution quantiles are obtained  \n",
                     "  by inverting cumulative distribution function of the \n",
                     "  non-central F-distribution given the observed F-stat.\n"))
        }
      
    } # verbose
    
    precision.power.out <- rbind(probs, precision.power.out)
    rownames(precision.power.out) <- c("probs", "quantile", effect, "power")
    colnames(precision.power.out) <- NULL
    
    return(invisible(as.data.frame(t(precision.power.out))))
    
  } else {
    
    stop(
      "Object should be a class of 'pwrss' and 'f'",
      call. = FALSE
    )
  }
  
} # precision.power.f.test()

precision.power.f <- precision.power.f.test



###################
# power.t.student #
# validated       #
###################

# library(devtools)
# install_github("metinbulus/hcttest")

library(hcttest)
delta.obj <- hc.t.test(m1 = 0.50, m2 = 0, sd1 = 1, sd2 = 1, n1 = 64, n2 = 64)
d.obj <- smd.hc.t.test(delta.obj)
d.obj$es

pwr.obj <- power.t.student(d = 0.50, n2 = 64)
pwr.obj.lower <- power.t.student(d = 0.1471161, n2 = 64)
pwr.obj.upper <- power.t.student(d = 0.8509597, n2 = 64)

rbind(c(pwr.obj.lower$ncp, pwr.obj$ncp, pwr.obj.upper$ncp),
      c(pwr.obj.lower$power, pwr.obj$power, pwr.obj.upper$power))

un.obj <- precision(pwr.obj)
t(un.obj)

ss.aipe.smd(delta = 0.50,          # Anticipated population Cohen's d
            width = 0.8509597 - 0.1471161, # Target CI full width
            conf.level = 0.95,    # 95% Confidence Interval
            assurance = 0.50000001,
            verify.ss = FALSE)     

power.t.student(d = 0.50, n2 = 64)

#################
# reg f test    #
# validated     #
#################

library(MBESS)
MBESS::precision.R2(R2 = 0.10, df.1 = 3, df.2 = 96, Random.Predictors = FALSE)

pwr.obj <- power.f.reg(r.squared = 0.10, k.total = 3, n = 100, utf = TRUE)
pwr.obj.lower <- power.f.reg(r.squared = 0.002865226, k.total = 3, n = 100)
pwr.obj.upper <- power.f.reg(r.squared = 0.2017249, k.total = 3, n = 100)

precision(pwr.obj)

rbind(c(pwr.obj.lower$ncp, pwr.obj$ncp, pwr.obj.upper$ncp),
      c(pwr.obj.lower$power, pwr.obj$power, pwr.obj.upper$power))


ss.aipe.R2(Population.R2=.10,
           conf.level=.95, 
           width=0.20172489 - 0.00286517,
           which.width="Full",
           p=3, 
           Random.Predictors=TRUE, 
           verify.ss=FALSE,
           degree.of.certainty = 0.50)

power.f.reg(r.squared = 0.07892815, k.total = 3, power = 0.80)


####################
# ancova f test    #
# validated        #
####################

library(MBESS)
MBESS::precision.R2(R2 = 0.10, df.1 = 3, df.2 = 96, Random.Predictors = FALSE)
R2 <- 0.10
df1 <- 3
df2 <- 96
F.value <- (R2 / df1) / ((1 - R2)/ df2)
MBESS::precision.omega2(F.value= F.value, df.1 = df1, df.2 = df2, N = 100)

pwr.obj <- power.f.ancova(eta.squared = 0.10, factor.levels = 4, n.total = 100)
pwr.obj.lower <- power.f.ancova(eta.squared = 0.002865226, factor.levels = 4, n.total = 100)
pwr.obj.upper <- power.f.ancova(eta.squared = 0.2017249, factor.levels = 4, n.total = 100)

precision(pwr.obj)

rbind(c(pwr.obj.lower$ncp, pwr.obj$ncp, pwr.obj.upper$ncp),
      c(pwr.obj.lower$power, pwr.obj$power, pwr.obj.upper$power))



ss.aipe.R2(Population.R2=.10,
           conf.level=.95, 
           width=0.208 - 0.003,
           which.width="Full",
           p=3, 
           Random.Predictors=FALSE, 
           verify.ss=FALSE,
           degree.of.certainty = 0.50)

width <- 0.208 - 0.003
width - 0.10

power.f.ancova(eta.squared = 0.05,
               null = 0, 
               factor.levels = 4,
               power = 0.50) |> precision()



################################################################################
# Monte Carlo simulation: mixed repeated-measures ANOVA
################################################################################

set.seed(1)

# --- 1. SIMULATION PARAMETERS ---

n_sim        <- 5000
n_per_group  <- 200

between_levels <- c("Control", "Treatment")
within_levels  <- paste0("Time", 1:3)

n_groups <- length(between_levels)
n_within <- length(within_levels)
n_total  <- n_per_group * n_groups

b_effect   <- 0.8
w_trend    <- 0.6
int_effect <- 0.5

subject_sd <- 1.0
error_sd   <- 0.8

# Storage
eta_b <- eta_w <- eta_i <- numeric(n_sim)
rho_within <- numeric(n_sim)
epsilon_w <- epsilon_i <- numeric(n_sim)

df_b_num <- df_b_den <- numeric(n_sim)
df_w_num <- df_w_den <- numeric(n_sim)
df_i_num <- df_i_den <- numeric(n_sim)

failed <- logical(n_sim)

################################################################################
# Helper: Greenhouse-Geisser epsilon
################################################################################

gg_epsilon <- function(wide, group = NULL) {
  
  if (!is.matrix(wide)) {
    wide <- as.matrix(wide)
  }
  
  k <- ncol(wide)
  
  if (k < 2L) {
    return(NA_real_)
  }
  
  if (is.null(group)) {
    
    S <- stats::cov(wide)
    
  } else {
    
    group <- factor(group)
    
    S_num <- matrix(0, nrow = k, ncol = k)
    S_den <- 0
    
    for (g in levels(group)) {
      
      Yg <- wide[group == g, , drop = FALSE]
      ng <- nrow(Yg)
      
      if (ng > 1L) {
        S_num <- S_num + (ng - 1) * stats::cov(Yg)
        S_den <- S_den + (ng - 1)
      }
    }
    
    if (S_den <= 0) {
      return(NA_real_)
    }
    
    S <- S_num / S_den
  }
  
  # Centering matrix: removes the grand repeated-measures component.
  C <- diag(k) - matrix(1 / k, nrow = k, ncol = k)
  S_centered <- C %*% S %*% C
  
  numerator <- sum(diag(S_centered))^2
  denominator <- (k - 1) * sum(S_centered^2)
  
  if (!is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }
  
  eps <- numerator / denominator
  
  # Bound epsilon to its theoretical range.
  eps <- max(1 / (k - 1), eps)
  eps <- min(1, eps)
  
  eps
}

################################################################################
# Helper: extract ANOVA quantities
################################################################################

extract_rm_anova <- function(dat) {
  
  fit <- stats::aov(
    dv ~ between_factor * within_factor + Error(id / within_factor),
    data = dat
  )
  
  s <- summary(fit)
  
  a1 <- s[[1]][[1]]   # Error: id
  a2 <- s[[2]][[1]]   # Error: id:within_factor
  
  SS_b    <- a1["between_factor", "Sum Sq"]
  SS_b_e  <- a1["Residuals", "Sum Sq"]
  df_b_n  <- a1["between_factor", "Df"]
  df_b_d  <- a1["Residuals", "Df"]
  
  SS_w    <- a2["within_factor", "Sum Sq"]
  SS_i    <- a2["between_factor:within_factor", "Sum Sq"]
  SS_w_e  <- a2["Residuals", "Sum Sq"]
  
  df_w_n  <- a2["within_factor", "Df"]
  df_w_d  <- a2["Residuals", "Df"]
  df_i_n  <- a2["between_factor:within_factor", "Df"]
  df_i_d  <- a2["Residuals", "Df"]
  
  list(
    eta_b = SS_b / (SS_b + SS_b_e),
    eta_w = SS_w / (SS_w + SS_w_e),
    eta_i = SS_i / (SS_i + SS_w_e),
    
    df_b_num = df_b_n,
    df_b_den = df_b_d,
    df_w_num = df_w_n,
    df_w_den = df_w_d,
    df_i_num = df_i_n,
    df_i_den = df_i_d
  )
}

################################################################################
# --- 2. MONTE CARLO LOOP ---
################################################################################

for (sim in seq_len(n_sim)) {
  
  tryCatch({
    
    # Subject-level data
    subject_id <- seq_len(n_total)
    
    subject_group <- rep(
      between_levels,
      each = n_per_group
    )
    
    subject_intercept <- rnorm(
      n_total,
      mean = 0,
      sd = subject_sd
    )
    
    # Long data
    id <- rep(subject_id, each = n_within)
    
    between_factor <- rep(
      subject_group,
      each = n_within
    )
    
    within_factor <- rep(
      within_levels,
      times = n_total
    )
    
    time_score <- rep(
      seq_len(n_within) - 1,
      times = n_total
    )
    
    subject_intercept_long <- rep(
      subject_intercept,
      each = n_within
    )
    
    # Mean structure
    mu <- 5 +
      (between_factor == "Treatment") * b_effect +
      time_score * w_trend +
      (between_factor == "Treatment" &
         within_factor == within_levels[n_within]) * int_effect
    
    dv <- mu +
      subject_intercept_long +
      rnorm(n_total * n_within, mean = 0, sd = error_sd)
    
    sim_data <- data.frame(
      id = factor(id),
      between_factor = factor(between_factor, levels = between_levels),
      within_factor = factor(within_factor, levels = within_levels),
      dv = dv
    )
    
    # Wide matrix: subjects x time points
    wide_matrix <- unclass(
      xtabs(dv ~ id + within_factor, data = sim_data)
    )
    
    # Subject-level group labels aligned with wide matrix
    subject_info <- sim_data[!duplicated(sim_data$id),
                             c("id", "between_factor")]
    
    subject_info <- subject_info[
      match(rownames(wide_matrix), subject_info$id),
    ]
    
    groups <- subject_info$between_factor
    
    # Mean within-subject correlation
    cor_matrix <- stats::cor(wide_matrix)
    rho_within[sim] <- mean(cor_matrix[lower.tri(cor_matrix)])
    
    # Greenhouse-Geisser epsilon, pooled across groups
    eps <- gg_epsilon(
      wide = wide_matrix,
      group = groups
    )
    
    epsilon_w[sim] <- eps
    epsilon_i[sim] <- eps
    
    # Repeated-measures ANOVA
    out <- extract_rm_anova(sim_data)
    
    eta_b[sim] <- out$eta_b
    eta_w[sim] <- out$eta_w
    eta_i[sim] <- out$eta_i
    
    df_b_num[sim] <- out$df_b_num
    df_b_den[sim] <- out$df_b_den
    
    df_w_num[sim] <- out$df_w_num
    df_w_den[sim] <- out$df_w_den
    
    df_i_num[sim] <- out$df_i_num
    df_i_den[sim] <- out$df_i_den
    
  }, error = function(e) {
    
    failed[sim] <<- TRUE
    
    eta_b[sim] <<- NA_real_
    eta_w[sim] <<- NA_real_
    eta_i[sim] <<- NA_real_
    
    rho_within[sim] <<- NA_real_
    epsilon_w[sim] <<- NA_real_
    epsilon_i[sim] <<- NA_real_
    
    df_b_num[sim] <<- NA_real_
    df_b_den[sim] <<- NA_real_
    df_w_num[sim] <<- NA_real_
    df_w_den[sim] <<- NA_real_
    df_i_num[sim] <<- NA_real_
    df_i_den[sim] <<- NA_real_
  })
}

################################################################################
# --- 3. BUILD EMPIRICAL MONTE CARLO RESULTS ---
################################################################################

summarise_effect <- function(eta, df_num, df_den, rho, epsilon) {
  
  data.frame(
    df_num = unique(stats::na.omit(df_num))[1],
    df_den = unique(stats::na.omit(df_den))[1],
    rho.within = mean(rho, na.rm = TRUE),
    epsilon = mean(epsilon, na.rm = TRUE),
    eta_squared = mean(eta, na.rm = TRUE),
    CI_lower = unname(stats::quantile(eta, 0.025, na.rm = TRUE)),
    CI_median = unname(stats::quantile(eta, 0.50, na.rm = TRUE)),
    CI_upper = unname(stats::quantile(eta, 0.975, na.rm = TRUE))
  )
}

between.effect.result <- summarise_effect(
  eta = eta_b,
  df_num = df_b_num,
  df_den = df_b_den,
  rho = rho_within,
  epsilon = rep(1, n_sim)
)

within.effect.result <- summarise_effect(
  eta = eta_w,
  df_num = df_w_num,
  df_den = df_w_den,
  rho = rho_within,
  epsilon = epsilon_w
)

interaction.effect.result <- summarise_effect(
  eta = eta_i,
  df_num = df_i_num,
  df_den = df_i_den,
  rho = rho_within,
  epsilon = epsilon_i
)

rownames(between.effect.result) <- "between_factor"
rownames(within.effect.result) <- "within_factor"
rownames(interaction.effect.result) <- "between_factor:within_factor"

monte_carlo_results <- rbind(
  between.effect.result,
  within.effect.result,
  interaction.effect.result
)

################################################################################
# --- 4. PRINT RESULTS ---
################################################################################

print(monte_carlo_results)

cat("\nFailed simulations:", sum(failed), "out of", n_sim, "\n")




# valdiate #####################################################################

pwr.obj <- power.f.mixed.anova(eta.squared = 0.16388400,
                               factor.levels = c(2,3),
                               effect = "between",
                               rho.within = NA,
                               n.total = n_total,
                               ceil.n = FALSE, 
                               verbose = 0)
precision(pwr.obj)
MBESS::precision.R2(R2 = 0.16388400, df.1 = pwr.obj$df1, df.2 = pwr.obj$df2, Random.Predictors = FALSE)[c(1,3)]
print(monte_carlo_results["between_factor",c("eta_squared", "CI_lower", "CI_median", "CI_upper")])


pwr.obj <- power.f.mixed.anova(eta.squared = 0.45506516,
                               factor.levels = c(2,3),
                               effect = "within",
                               rho.within = NA,
                               # rho.within = 0.632544,
                               epsilon = 0.9950807, # eps_GG, eps_HF
                               n.total = n_total, 
                               ceil.n = FALSE, 
                               verbose = 0)
precision(pwr.obj)
MBESS::precision.R2(R2 = 0.45506516, df.1 = pwr.obj$df1, df.2 = pwr.obj$df2, Random.Predictors = FALSE)[c(1,3)]
print(monte_carlo_results["within_factor", c("eta_squared", "CI_lower", "CI_median", "CI_upper")])



pwr.obj <- power.f.mixed.anova(eta.squared = 0.03386145 ,
                               factor.levels = c(2,3),
                               effect = "interaction",
                               rho.within = NA,
                               #rho.within = 0.632544,
                               epsilon = 0.9950807,
                               n.total = n_total,
                               ceil.n = FALSE, 
                               utf = TRUE,
                               verbose = 0)
precision(pwr.obj)
MBESS::precision.R2(R2 = 0.03386145 , df.1 = pwr.obj$df1, df.2 = pwr.obj$df2, Random.Predictors = FALSE)[c(1,3)]
print(monte_carlo_results["between_factor:within_factor", c("eta_squared", "CI_lower", "CI_median", "CI_upper")])





################################################################################
# single data
################################################################################

# Example dataset from your simulation structure
set.seed(1)

n_per_group <- 100
n_total     <- n_per_group * 2
n_within    <- 3

id <- rep(1:n_total, each = n_within)
between_factor <- rep(c("Control", "Treatment"), each = n_per_group * n_within)
within_factor  <- rep(c("Time1", "Time2", "Time3"), times = n_total)

subject_intercept <- rep(rnorm(n_total, 0, 1), each = n_within)

mu <- 5 +
  (between_factor == "Treatment") * 0.6 +
  (as.numeric(factor(within_factor)) * 0.5) +
  (between_factor == "Treatment" & within_factor == "Time3") * 0.4

dv <- mu + subject_intercept + rnorm(n_total * n_within, 0, 0.8)

dat <- data.frame(
  id = factor(id),
  between_factor = factor(between_factor),
  within_factor = factor(within_factor),
  dv = dv
)

# Base R repeated-measures ANOVA
fit <- aov(dv ~ between_factor + within_factor + between_factor * within_factor + Error(id/within_factor), data = dat)
summary(fit)

# eta-squared values ############################################################

a1 <- summary(fit)[[1]][[1]]   # between-subjects
a2 <- summary(fit)[[2]][[1]]   # within-subjects

# Sum of squares
SS_b  <- a1["between_factor", "Sum Sq"]
SS_be <- a1["Residuals", "Sum Sq"]

SS_w  <- a2["within_factor", "Sum Sq"]
SS_i  <- a2["between_factor:within_factor", "Sum Sq"]
SS_we <- a2["Residuals", "Sum Sq"]

# Partial eta squared
eta_b <- SS_b / (SS_b + SS_be)
eta_w <- SS_w / (SS_w + SS_we)
eta_i <- SS_i / (SS_i + SS_we)

eta_b; eta_w; eta_i



# epsilon #####################################################################


# Convert long data to wide matrix: subjects × timepoints
wide <- unclass(xtabs(dv ~ id + within_factor, data = dat))

# Subject-level group labels
subject_info <- dat[!duplicated(dat$id), c("id", "between_factor")]
subject_info <- subject_info[match(rownames(wide), subject_info$id), ]

# Pooled within-group covariance matrix
groups <- subject_info$between_factor

S_pooled <- Reduce(
  "+",
  lapply(levels(groups), function(g) {
    Yg <- wide[groups == g, , drop = FALSE]
    (nrow(Yg) - 1) * cov(Yg)
  })
) / (nrow(wide) - nlevels(groups))

k <- ncol(S_pooled)

C <- diag(k) - matrix(1 / k, nrow = k, ncol = k)

S_centered <- C %*% S_pooled %*% C

eps_GG <- sum(diag(S_centered))^2 /
  ((k - 1) * sum(S_centered^2))

eps_GG <- min(1, max(1 / (k - 1), eps_GG))

eps_GG



N <- nrow(wide)
r_x <- nlevels(groups)   # intercept + group effect rank = 2
d <- k - 1

eps_HF <- ((N * d * eps_GG) - 2) /
  (d * (N - r_x) - d^2 * eps_GG)

eps_HF <- min(1, eps_HF)
eps_HF <- max(1 / d, eps_HF)

eps_HF



# bootstrap standard errors

boot_eta <- function(dat, B = 2000) {
  
  eta_b <- eta_w <- eta_i <- numeric(B)
  
  subject_info <- dat[!duplicated(dat$id), c("id", "between_factor")]
  
  for (b in seq_len(B)) {
    
    boot_parts <- list()
    new_id <- 1L
    
    for (g in levels(subject_info$between_factor)) {
      
      ids_g <- subject_info$id[subject_info$between_factor == g]
      samp_ids <- sample(ids_g, size = length(ids_g), replace = TRUE)
      
      for (sid in samp_ids) {
        
        tmp <- dat[dat$id == sid, ]
        tmp$id <- factor(new_id)
        boot_parts[[length(boot_parts) + 1L]] <- tmp
        
        new_id <- new_id + 1L
      }
    }
    
    boot_dat <- do.call(rbind, boot_parts)
    
    fit_b <- aov(
      dv ~ between_factor * within_factor + Error(id / within_factor),
      data = boot_dat
    )
    
    s <- summary(fit_b)
    
    a1b <- s[[1]][[1]]
    a2b <- s[[2]][[1]]
    
    SS_b  <- a1b["between_factor", "Sum Sq"]
    SS_be <- a1b["Residuals", "Sum Sq"]
    
    SS_w  <- a2b["within_factor", "Sum Sq"]
    SS_i  <- a2b["between_factor:within_factor", "Sum Sq"]
    SS_we <- a2b["Residuals", "Sum Sq"]
    
    eta_b[b] <- SS_b / (SS_b + SS_be)
    eta_w[b] <- SS_w / (SS_w + SS_we)
    eta_i[b] <- SS_i / (SS_i + SS_we)
  }
  
  list(
    between = quantile(eta_b, c(.025, .975), na.rm = TRUE),
    within  = quantile(eta_w, c(.025, .975), na.rm = TRUE),
    inter   = quantile(eta_i, c(.025, .975), na.rm = TRUE)
  )
}


ci <- boot_eta(dat, B = 5000)

ci

eta_b; eta_w; eta_i


# valdiate #####################################################################

pwr.obj <- power.f.mixed.anova(eta.squared = eta_b,
                               factor.levels = c(2,3),
                               effect = "between",
                               rho.within = NA,
                               n.total = n_total,
                               ceil.n = FALSE, 
                               verbose = 0)
precision(pwr.obj)
ci$between
MBESS::precision.R2(R2 = eta_b, df.1 = pwr.obj$df1, df.2 = pwr.obj$df2, Random.Predictors = FALSE)[c(1,3)]


pwr.obj <- power.f.mixed.anova(eta.squared = eta_w,
                               factor.levels = c(2,3),
                               effect = "within",
                               rho.within = NA,
                               # rho.within = 0.632544,
                               epsilon = eps_GG, # eps_GG, eps_HF
                               n.total = n_total, 
                               ceil.n = FALSE, 
                               verbose = 0)
precision(pwr.obj)
ci$within
MBESS::precision.R2(R2 = eta_w, df.1 = pwr.obj$df1, df.2 = pwr.obj$df2, Random.Predictors = FALSE)[c(1,3)]



pwr.obj <- power.f.mixed.anova(eta.squared = eta_i,
                               factor.levels = c(2,3),
                               effect = "interaction",
                               rho.within = NA,
                               #rho.within = 0.632544,
                               epsilon = eps_GG,
                               n.total = n_total,
                               ceil.n = FALSE, 
                               verbose = 0)
t(precision(pwr.obj))
ci$inter
MBESS::precision.R2(R2 = eta_i, df.1 = pwr.obj$df1, df.2 = pwr.obj$df2, Random.Predictors = FALSE)[c(1,3)]

n.total <- pwr.obj$n.total
eta.squared <- pwr.obj$eta.squared
ncp <- pwr.obj$ncp
null.ncp <- pwr.obj$null.ncp
epsilon <- pwr.obj$parms$epsilon
f.squared <- ncp / (n.total * epsilon)
# f.squared <- eta.squared / (1 - eta.squared)
f.statistic <- f.squared * (df2 / df1)

