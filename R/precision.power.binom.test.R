# Clopper, C. J., & Pearson, E. S. (1934). 
# The use of confidence or fiducial limits illustrated in the case of the binomial. 
# Biometrika, 26(4), 404–413. https://doi.org/10.2307/2331986 

# statistic: observed test statistic, which is a proportion 
# probs: inversion probabilities
# set statistics = c(0.025, 0.975) for 95% CI
precision.binom.test <- function(statistic, size, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size < 1 || size != round(size)) {
    stop("'size' must be one positive integer.", call. = FALSE)
  }
  
  if (!is.numeric(statistic) || length(statistic) != 1L || !is.finite(statistic) || statistic < 0 || statistic > 1) {
    stop("'statistic' must be one finite number between 0 and 1.", call. = FALSE)
  }
  
  if (!is.numeric(probs) || any(!is.finite(probs)) || any(is.na(probs)) || any(probs < 0.000001) || any(probs > 0.999999)) {
    stop("'probs' must be between 0.000001 and 0.999999.", call. = FALSE)
  }
  
  size <- as.integer(round(size))
  x.raw <- statistic * size
  x <- as.integer(round(x.raw))
  
  # generalized clopper–pearson quantiles
  ci <- numeric(length(probs))
  
  lower <- probs < 0.50
  upper <- probs > 0.50
  middle <- probs == 0.50
  
  # lower-tail confidence quantiles from inversion probs
  if (any(lower)) {
    ci[lower] <- if (x == 0L) {
      0
    } else {
      stats::qbeta(probs[lower], shape1 = x, shape2 = size - x + 1)
    }
  }
  
  # upper-tail confidence quantiles from inversion probs
  if (any(upper)) {
    ci[upper] <- if (x == size) {
      1
    } else {
      stats::qbeta(probs[upper], shape1 = x + 1, shape2 = size - x)
    }
  }
  
  # convention for the center
  ci[middle] <- x / size
  
  names(ci) <- paste0(round(probs * 100, 1), "%")
  
  list(
    statistic = statistic,
    probs = probs,
    ci = ci
  )
  
} # precision.binom.test()

precision.binom <- precision.binom.test


precision.power.binom.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = TRUE, utf = FALSE) {
  
  if (all(c("pwrss", "binom") %in% class(x))) {
    
    statistic <- x$prob
    size <- x$size
    null.prob <- x$null.prob
    alternative <- x$alternative
    
    if(verbose > 0 && length(probs) > 10)
      stop("Printing is not allowed with large output. Try verbose = FALSE.",
           call. = FALSE)
    
    conf.int <- precision.binom.test(
      statistic = statistic,
      size = x$size,
      probs = probs
    )$ci
    
    if(any(is.na(conf.int))) 
      stop("Calculating confidence interval for power is not possible with the current bounds.",
           call. = FALSE)
    
    power <- vapply(
      conf.int,
      function(x) {
        tryCatch({
          power.binom.test(
            prob = x,
            null.prob = null.prob,
            size = size,
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
    
    precision.power.out <- data.frame(rbind(conf.int, power))
    colnames(precision.power.out) <-  paste0(round(probs * 100, 1), "%")
    rownames(precision.power.out) <- c("  Prop. Uncertainty Distribution", "  Conditional Statistical Power")
    
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
                     "\n  Uncertainty distribution quantiles of proportions are  \n",
                     "  obtained directly from the inverse cumulative dist. \n",
                     "  function of the beta-distribution given observed stat. (prop.), \n",
                     "  in line with Clopper and Pearson (1934).\n",
                     "\033[0m"))
        } else {
          cat(paste0("\n  Uncertainty distribution quantiles of proportions are  \n",
                     "  obtained directly from the inverse cumulative dist. \n",
                     "  function of the beta-dist. given observed stat. (prop.), \n",
                     "  in line with Clopper and Pearson (1934).\n"))
        }
      
    } # verbose
    
    precision.power.out <- rbind(probs, precision.power.out)
    rownames(precision.power.out) <- c("probs", "quantile", "power")
    colnames(precision.power.out) <- NULL
    
    return(invisible(as.data.frame(t(precision.power.out))))
    
  } else {
    
    stop(
      "Object should be a class of 'pwrss' and 'binom'",
      call. = FALSE
    )
  }
  
} # precision.power.binom.test()

precision.power.binom <- precision.power.binom.test
