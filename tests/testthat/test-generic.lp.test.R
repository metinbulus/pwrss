test_that("generic.lp.test.R works", {
    # power.lp.test ----------------------------------------------------------------------------------------------------
    expect_equal(power.lp.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", verbose = 0),
                 list(alternative = "two.sided", ncp = 1.96, null.ncp = 0, df = 100, alpha = 0.05,
                      t.alpha = 1.959964 * c(-1, 1), beta = 0.50187931, type.s = 0.000104670166, type.m = 1.410027,
                      power = 0.49812069))
    expect_equal(power.lp.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "one.sided", verbose = 0),
                 list(alternative = "one.sided", ncp = 1.96, null.ncp = 0, df = 100, alpha = 0.05, t.alpha = 1.64485363,
                      beta = 0.37930952, type.s = 0, type.m = NA, power = 0.62069048))
    expect_equal(power.lp.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05, alternative = "two.one.sided", verbose = 0),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.33388239 * c(-1, 1), beta = 0.73846831, type.s = NA, type.m = NA, power = 0.261531695))
    expect_equal(power.lp.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05, alternative = "two.one.sided", verbose = 0),
                 list(alternative = "two.one.sided", ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
                      t.alpha = 2.96236048 * c(-1, 1), beta = 0.83093024, type.s = NA, type.m = NA, power = 0.16906976))
    expect_warning(power.lp.test(ncp = 36, df = 100, alpha = 0.05, alternative = "two.sided", verbose = 0),
                   "Consider using a z-test. Lambda-prime distribution with a large non-centrality parameter can be unreliable.")

    # ncp.z.test -------------------------------------------------------------------------------------------------------
    expect_equal(ncp.lp.test(power = 0.80, df = 100, alpha = 0.05, alternative = "two.sided"),
                 list(alternative = "two.sided", ncp = 2.82523051, null.ncp = 0, df = 100, alpha = 0.05,
                      t.alpha = 1.959964 * c(-1, 1), beta = 0.200005209, type.s = 0.0000013540822, type.m = 1.12380496,
                      power = 0.7999948))
    expect_equal(ncp.lp.test(power = 0.80, df = 100, alpha = 0.05, alternative = "one.sided"),
                 list(alternative = "one.sided", ncp = 2.50583448, null.ncp = 0, df = 100, alpha = 0.05,
                      t.alpha = 1.64485363, beta = 0.199997966, type.s = 0, type.m = NA, power = 0.800002034))
    expect_equal(ncp.lp.test(power = 0.80, req.sign = "0", null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                             alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.33388239 * c(-1, 1), beta = 0.73846831, type.s = NA, type.m = NA, power = 0.261531695))
    expect_equal(ncp.lp.test(power = 0.80, req.sign = "+", null.ncp = c(-1, 1), df = 100, alpha = 0.05,
                             alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = 3.84409869, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
                      t.alpha = 2.96236048 * c(-1, 1), beta = 0.200002176, type.s = NA, type.m = NA, power = 0.799997824))
    expect_equal(ncp.lp.test(power = 0.80, req.sign = "-", null.ncp = c(-1, 1), df = 100, alpha = 0.05,
                             alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = -3.84409869, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
                      t.alpha = 2.96236048 * c(-1, 1), beta = 0.200002176, type.s = NA, type.m = NA, power = 0.799997824))
    expect_error(ncp.lp.test(power = 0.80),
                 "Exactly one of the parameters `ncp` or `df` must be given, one has to be NULL.")
    expect_error(ncp.lp.test(power = 0.80, ncp = 3, alpha = 0.05, alternative = "two.sided"),
                 "Solving for degrees of freedom is currently not allowed due to numerical instability in PDQutils::AS269 function.")
    expect_error(ncp.lp.test(power = 0.80, df = 2), "`df` can not be smaller than 3.")
})
