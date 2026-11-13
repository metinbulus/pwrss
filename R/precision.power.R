# double check how equivalence and minimum effect tests are handled since they have two null values
ci.power <- function(x, probs = c(0.025, 0.50, 0.975), verbose = 1, utf = FALSE) { 
  
  if ("z" %in% class(x)) {
    ci.power.z.test(x, probs = probs, verbose = verbose, utf = utf)
  } else if ("t" %in% class(x)) {
    ci.power.t.test(x, probs = probs, verbose = verbose, utf = utf)
  } else if ("lp" %in% class(x)) {
    ci.power.lp.test(x, probs = probs, verbose = verbose, utf = utf)
  } else if ("f" %in% class(x)) {
    ci.power.f.test(x, probs = probs, verbose = verbose, utf = utf)
  } else if ("chisq" %in% class(x)) {
    ci.power.chisq.test(x, probs = probs, verbose = verbose, utf = utf)
  } else if ("binom" %in% class(x)) {
    ci.power.binom.test(x, probs = probs, verbose = verbose, utf = utf)
  } else if ("exact" %in% class(x)) {
    stop("Uncertainty quantiles are not available for exact tests at the moment.", call. = TRUE)
  } else {
    stop("The object `x` should inherit class 'pwrss' and either 't', 'z', 'lp', 'f', 'chisq', or 'binom'.", call. = FALSE)
  }
  
} # ci.power
