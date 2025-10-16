test_that("power.z.twoprop works", {
  # power
  resLst <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", verbose = FALSE)
  outSmp <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided"))
  outPty <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, n2 = 500, alternative = "one.sided", pretty = TRUE))
  expect_equal(resLst[["parms"]],
               list(prob1 = 0.65, prob2 = 0.6, margin = 0, n2 = 500, n.ratio = 1, alpha = 0.05, power = 0.4952621,
                    arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, alternative = "one.sided",
                    ceiling = TRUE, verbose = TRUE),
               tolerance = 1e-6)
  expect_equal(resLst[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
               list(test = "z", delta = 0.05, odds.ratio = 1.238095, mean = 1.635175, sd = 1, null.mean = 0, null.sd = 1.001336,
                    z.alpha = 1.647051, power = 0.4952621, n = c(n1 = 500, n2 = 500), n.total = 1000),
               tolerance = 1e-6)
  expect_equal(class(resLst), c("pwrss", "z", "twoprops"))
  expect_equal(outSmp,
               c("+--------------------------------------------------+",
                 "|                POWER CALCULATION                 |",
                 "+--------------------------------------------------+", "",
                 "Independent Proportions", "",
                 "  Method          : Normal Approximation", "",
                 "---------------------------------------------------",
                 "Hypotheses",
                 "---------------------------------------------------",
                 "  H0 (Null Claim) : prob1 - prob2 <= 0 ",
                 "  H1 (Alt. Claim) : prob1 - prob2 > 0 ", "",
                 "---------------------------------------------------",
                 "Results",
                 "---------------------------------------------------",
                 "  Sample Size          = 500 and 500",
                 "  Type 1 Error (alpha) = 0.050",
                 "  Type 2 Error (beta)  = 0.505",
                 "  Statistical Power    = 0.495  <<", ""))
  expect_equal(outPty,
               c("╔══════════════════════════════════════════════════╗",
                 "║               \033[34m POWER CALCULATION \033[0m                ║",
                 "╚══════════════════════════════════════════════════╝", "",
                 "Independent Proportions", "",
                 "  Method            : Normal Approximation", "",
                 "───────────────────────────────────────────────────",
                 "Hypotheses",
                 "───────────────────────────────────────────────────",
                 "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                 "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                 "───────────────────────────────────────────────────",
                 "Results",
                 "───────────────────────────────────────────────────",
                 "  Sample Size       = 500 and 500 ",
                 "  Type 1 Error (α)  = 0.050",
                 "  Type 2 Error (β)  = 0.505",
                 " \033[34m Statistical Power = 0.495\033[0m\033[1;35m  ◄◄ ", "", "\033[0m"))
  # sample size
  resLst <- power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", verbose = FALSE)
  outSmp <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided"))
  outPty <- capture.output(power.z.twoprops(prob1 = 0.65, prob2 = 0.60, alpha = 0.05, power = 0.80, alternative = "one.sided", pretty = TRUE))
  expect_equal(resLst[["parms"]],
               list(prob1 = 0.65, prob2 = 0.6, margin = 0, n2 = 1159, n.ratio = 1, alpha = 0.05, power = 0.800246,
                    arcsine = FALSE, correct = FALSE, paired = FALSE, rho.paired = 0.5, alternative = "one.sided",
                    ceiling = TRUE, verbose = TRUE),
               tolerance = 1e-6)
  expect_equal(resLst[c("test", "delta", "odds.ratio", "mean", "sd", "null.mean", "null.sd", "z.alpha", "power", "n", "n.total")],
               list(test = "z", delta = 0.05, odds.ratio = 1.238095, mean = 2.48955, sd = 1, null.mean = 0, null.sd = 1.001336,
                    z.alpha = 1.647051, power = 0.800246, n = c(n1 = 1159, n2 = 1159), n.total = 2318),
               tolerance = 1e-6)
  expect_equal(class(resLst), c("pwrss", "z", "twoprops"))
  expect_equal(outSmp,
               c("+--------------------------------------------------+",
                 "|             SAMPLE SIZE CALCULATION              |",
                 "+--------------------------------------------------+", "",
                 "Independent Proportions", "",
                 "  Method          : Normal Approximation", "",
                 "---------------------------------------------------",
                 "Hypotheses",
                 "---------------------------------------------------",
                 "  H0 (Null Claim) : prob1 - prob2 <= 0 ",
                 "  H1 (Alt. Claim) : prob1 - prob2 > 0 ", "",
                 "---------------------------------------------------",
                 "Results",
                 "---------------------------------------------------",
                 "  Sample Size          = 1159 and 1159  <<",
                 "  Type 1 Error (alpha) = 0.050",
                 "  Type 2 Error (beta)  = 0.200",
                 "  Statistical Power    = 0.8", ""))
  expect_equal(outPty,
               c("╔══════════════════════════════════════════════════╗",
                 "║           \033[34m SAMPLE SIZE CALCULATION \033[0m              ║",
                 "╚══════════════════════════════════════════════════╝", "",
                 "Independent Proportions", "",
                 "  Method            : Normal Approximation", "",
                 "───────────────────────────────────────────────────",
                 "Hypotheses",
                 "───────────────────────────────────────────────────",
                 "  H₀ (Null)         : P₁ - P₂ ≤ 0 ",
                 "  H₁ (Alternative)  : P₁ - P₂ > 0 ", "",
                 "───────────────────────────────────────────────────",
                 "Results",
                 "───────────────────────────────────────────────────",
                 "\033[34m  Sample Size       = 1159 and 1159\033[0m\033[1;35m  ◄◄ ",
                 "\033[0m  Type 1 Error (α)  = 0.050",
                 "  Type 2 Error (β)  = 0.200",
                 "  Statistical Power = 0.8", ""))
})
