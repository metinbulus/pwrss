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


precision.power.f.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = TRUE) {
  
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
