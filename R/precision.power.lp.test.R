# statistic: observed t-statistic
# probs: quantile probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.lp.test <- function(statistic, df, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  
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
  
  ci <- tryCatch(
    qlambdap(
      p = probs,
      df = df,
      t = statistic,
      lower.tail = TRUE,
      log.p = FALSE,
      order.max = 6L
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  if (any(abs(ci) > 35, na.rm = TRUE)) {
    warning(
      paste(
        "At least one confidence limit exceeds 35 in magnitude;",
        "base R noncentral-t probabilities may be inaccurate."
      ),
      call. = FALSE
    )
  }
  
  names(ci) <- paste0(round(probs * 100, 1), "%")

  list(
    statistic = statistic,
    df = df,
    probs = probs,
    ci = ci
  )
  
} # precision.lp.test()

precision.lp <- precision.lp.test


precision.power.lp.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = TRUE, utf = FALSE) {
  
  if (all(c("pwrss", "lp") %in% class(x))) {

    statistic <- x$ncp
    null.ncp <- x$null.ncp
    alternative <- x$alternative
    df <- x$df
    
    if (verbose > 0 && length(probs) > 10)
      stop("Printing is not allowed with large output. Try verbose = FALSE.",
           call. = FALSE)
    
    conf.int <- precision.lp.test(
      statistic = statistic,
      df = df,
      probs = probs
    )$ci
    
    if(any(is.na(conf.int))) 
      stop("Calculating confidence interval for power is not possible with the current bounds.",
           call. = FALSE)
    
    power <- vapply(
      conf.int,
      function(x) {
        tryCatch({
          power.lp.test(
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
                     "  directly from the cumulative distribution function of the \n",
                     "  non-central LP-distribution given the observed LP-stat.\n",
                     "\033[0m"))
        } else {
          cat(paste0("\n  NCP uncertainty distribution quantiles are obtained \n",
                     "  directly from the cumulative distribution function of the \n",
                     "  non-central LP-distribution given the observed LP-stat.\n"))
        }

    } # verbose
    
    precision.power.out <- rbind(probs, precision.power.out)
    rownames(precision.power.out) <- c("probs", "quantile", "power")
    colnames(precision.power.out) <- NULL
    
    return(invisible(as.data.frame(t(precision.power.out))))
    
  } else {
    
    stop(
      "Object should be a class of 'pwrss' and 'lp'",
      call. = FALSE
    )
  }
  
} # precision.power.lp.test()

precision.power.lp <- precision.power.lp.test
