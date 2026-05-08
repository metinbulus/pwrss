test_that("generic.chisq.test.R works", {
    # power.chisq.test -------------------------------------------------------------------------------------------------
    expect_equal(power.chisq.test(ncp = 20, df = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.381376391, ncp = 20, null.ncp = 0, alpha = 0.05, df = 100, chisq.alpha = 124.342113))
    expect_equal(power.chisq.test(ncp = 20, null.ncp = 10, df = 10, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.31365895, ncp = 20, null.ncp = 10, alpha = 0.05, df = 10, chisq.alpha = 34.0886349))
    expect_equal(power.chisq.test(ncp = 20, df = 17, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.80744863, ncp = 20, null.ncp = 0, alpha = 0.05, df = 17, chisq.alpha = 27.5871116))
    expect_error(power.chisq.test(ncp = 1, null.ncp = 11, df = 17, alpha = 0.05, plot = FALSE, verbose = 0),
                 "`ncp` should be greater than or equal to `null.ncp`.")

    # ncp.chisq.test ---------------------------------------------------------------------------------------------------
    expect_equal(ncp.chisq.test(ncp = 40, df = NULL, alpha = 0.05),
                 list(power = 0.8, ncp = 40, null.ncp = 0, alpha = 0.05, df = 96.872062, chisq.alpha = 120.846553))
    expect_equal(ncp.chisq.test(ncp = NULL, df = 100, alpha = 0.05),
                 list(power = 0.8, ncp = 40.5564, null.ncp = 0, alpha = 0.05, df = 100, chisq.alpha = 124.342113))
    expect_error(ncp.chisq.test(ncp = NULL, df = NULL, alpha = 0.05),
                 "Exactly one of the parameters `ncp` or `df` must be given, one has to be NULL.")
    expect_error(ncp.chisq.test(ncp = NULL, df = 0.1, alpha = 0.05),
                 "Degrees of freedom can not be smaller than 1.")
})
