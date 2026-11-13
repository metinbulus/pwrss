# statistic: observed chi-square test statistic
# probs: inversion probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.chisq.test <- function(statistic, df, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  
  if (!is.numeric(statistic) ||
      length(statistic) != 1L ||
      !is.finite(statistic) ||
      statistic < 0) {
    stop("'statistic' must be one nonnegative finite number.",
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
        stats::pchisq(statistic, df = df, ncp = ncp)
      ),
      error = function(e) NA_real_
    )
  }
  
  find.ncp.chisq <- function(target, no.root = 0) {
    
    # Function whose root we want:
    # pchisq(...) = target
    objective <- function(ncp) {
      suppressWarnings(
        stats::pchisq(
          statistic,
          df = df,
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
    
  } # find.ncp.chisq
  
  ci <- vapply(
    1 - probs,
    function(target) {
      find.ncp.chisq(
        target = target
      )
    },
    numeric(1L)
  )
  
  names(ci) <- paste0(round(probs * 100, 1), "%")
  
  list(
    statistic = statistic,
    df = df,
    probs = probs,
    ci = ci
  )
  
} # precision.chisq.test()

precision.chisq <- precision.chisq.test


precision.power.chisq.test <- function(x, probs = c(0.025, 0.50, 0.975) , verbose = TRUE, utf = FALSE) {
  
  if(all(c("pwrss", "chisq") %in% class(x))) {
    
    statistic <- x$ncp
    null.ncp <- x$null.ncp
    df <- x$df

    if(verbose > 0 && length(probs) > 10)
      stop("Printing is not allowed with large output. Try verbose = FALSE.",
           call. = FALSE)
    
    conf.int <- precision.chisq.test(statistic = statistic, df = df, probs = probs)$ci
    
    if(any(is.na(conf.int)) || all(conf.int == 0)) 
      stop("Calculating confidence interval for power is not possible with the current bounds.",
           call. = FALSE)
    
    conf.int[which(conf.int == 0)] <- 1e-10
    
    power <- vapply(
      conf.int,
      function(x) {
        tryCatch({
          power.chisq.test(ncp = x, 
                           null.ncp = null.ncp,
                           df = df,
                           verbose = 0, 
                           plot = FALSE)$power
        }, error = function(e) {
          NA_real_
        })
      },
      numeric(1L)
    )
    
    precision.power.out <- data.frame(rbind(conf.int, power))
    colnames(precision.power.out) <-  paste0(round(probs * 100, 1), "%")
    rownames(precision.power.out) <- c("  NCP Uncertainty Distribution", "  Conditional Statistical Power")
    
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
                     "  non-central F-distribution given the observed F-statistic.\n",
                     "\033[0m"))
        } else {
          cat(paste0("\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  by inverting cumulative distribution function of the \n",
                     "  non-central F-distribution given the observed F-statistic.\n"))
        }
        
    } # verbose
    
    precision.power.out <- rbind(probs, precision.power.out)
    rownames(precision.power.out) <- c("probs", "quantile", "power")
    colnames(precision.power.out) <- NULL
    
    return(invisible(as.data.frame(t(precision.power.out))))
    
  } else {
    
    stop("Object should be a class of 'pwrss' and 'chisq'", call. = FALSE)
    
  }
  
} # precision.power.chisq.test

precision.power.chisq <- precision.power.chisq.test
