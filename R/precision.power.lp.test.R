# statistic: observed t-statistic
# probs: quantile probabilities
# set probs = c(0.025, 0.975) for 95% CI
precision.lp.test <- function(statistic, df, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  check.numeric(statistic)
  check.positive(df)
  check.vector(probs, check.proportion)
  
  ci <- tryCatch(qlambdap(p = probs, df = df, ncp = statistic, lower.tail = TRUE, log.p = FALSE),
                 error = function(e) rep(NA_real_, length(probs)))
  
  if (any(abs(ci) > 35, na.rm = TRUE)) {
    warning("At least one confidence limit exceeds 35 in magnitude; base R noncentral-t probabilities may be inaccurate.",
            call. = FALSE)
  }

  list(statistic = statistic, df = df, probs = probs, ci = setNames(ci, paste0(round(probs * 100, 1), "%")))
  
} # precision.lp.test()

precision.lp <- precision.lp.test


precision.power.lp.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = 1, utf = FALSE) {
  
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
    
    if (any(is.na(conf.int))) 
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
      print(precision.power.out, digits = 3)
      if (verbose > 1)
        if (utf) {
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
