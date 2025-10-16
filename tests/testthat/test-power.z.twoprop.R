test_that("power.z.twoprop works", {
  # power
  crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = FALSE)
  expect_equal(crrRes[["parms"]],
               list(prob1 = 0.65, prob2 = 0.6, margin = 0, n2 = 500, n.ratio = 1, alpha = 0.05, power = 0.4952621,
                    arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, alternative = "one.sided",
                    ceiling = TRUE, verbose = TRUE),
               tolerance = 1e-6)
  expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
               list(test = "z", delta = 0.05, odds.ratio = 1.238095, mean = 1.635175, sd = 1, null.mean = 0, null.sd = 1.001336,
                    z.alpha = 1.647051, power = 0.4952621, n = c(n1 = 500, n2 = 500), n.total = 1000),
               tolerance = 1e-6)
  expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
  # sample size
  crrRes <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = FALSE)
  expect_equal(crrRes[["parms"]],
               list(prob1 = 0.65, prob2 = 0.6, margin = 0, n2 = 1159, n.ratio = 1, alpha = 0.05, power = 0.800246,
                    arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, alternative = "one.sided",
                    ceiling = TRUE, verbose = TRUE),
               tolerance = 1e-6)
  expect_equal(crrRes[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
               list(test = "z", delta = 0.05, odds.ratio = 1.238095, mean = 2.48955, sd = 1, null.mean = 0, null.sd = 1.001336,
                    z.alpha = 1.647051, power = 0.800246, n = c(n1 = 1159, n2 = 1159), n.total = 2318),
               tolerance = 1e-6)
  expect_equal(class(crrRes), c("pwrss", "z", "twoprops"))
})
