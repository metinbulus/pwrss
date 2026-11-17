# Clopper, C. J., & Pearson, E. S. (1934). 
# The use of confidence or fiducial limits illustrated in the case of the binomial. 
# Biometrika, 26(4), 404–413. https://doi.org/10.2307/2331986 

# statistic: observed test statistic, which is a proportion 
# probs: inversion probabilities
# set statistics = c(0.025, 0.975) for 95% CI
precision.binom.test <- function(statistic, size, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) {
  check.proportion(statistic)
  check.size(size)
  check.vector(probs, check.proportion)
  
  x <- as.integer(round(statistic * size))
  
  # generalized clopper–pearson quantiles
  ci <- numeric(length(probs))
  
  lower <- probs < 0.50
  upper <- probs > 0.50
 
  # lower-tail, upper-tail and centre confidence quantiles from inversion probs
  if (any(lower)) ci[lower] <- if (x == 0L)   0 else stats::qbeta(probs[lower], shape1 = x, shape2 = size - x + 1)
  if (any(upper)) ci[upper] <- if (x == size) 1 else stats::qbeta(probs[upper], shape1 = x + 1, shape2 = size - x)
  ci[probs == 0.50] <- x / size
  
  list(statistic = statistic, probs = probs, ci = setNames(ci, paste0(round(probs * 100, 1), "%")))
} # precision.binom.test()

precision.binom <- precision.binom.test


precision.power.binom.test <- function(x, probs = c(0.025, 0.50, 0.975), verbose = 0, utf = FALSE) {
  
  if (all(c("pwrss", "binom") %in% class(x))) {
    
    statistic <- x$prob
    size <- x$size
    null.prob <- x$null.prob
    alternative <- x$alternative
    
    if (verbose > 0 && length(probs) > 10)
      stop("Printing is not allowed with large output. Try verbose = 0.", call. = FALSE)
    
    conf.int <- precision.binom.test(statistic = statistic, size = x$size, probs = probs)$ci
    
    if (any(is.na(conf.int))) 
      stop("Calculating confidence interval for power is not possible with the current bounds.", call. = FALSE)
    
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
    
    invisible(as.data.frame(t(precision.power.out)))
    
  } else {

    stop("Object should be a class of 'pwrss' and 'binom'", call. = FALSE)

  }
  
} # precision.power.binom.test()

precision.power.binom <- precision.power.binom.test
