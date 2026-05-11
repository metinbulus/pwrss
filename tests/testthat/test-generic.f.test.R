test_that("generic.f.test.R works", {
    # power.f.test -----------------------------------------------------------------------------------------------------
    expect_equal(power.f.test(ncp = 2, df1 = 4, df2 = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.165090262, ncp = 2, null.ncp = 0, df1 = 4, df2 = 100, alpha = 0.05, f.alpha = 2.4626149))
    expect_equal(power.f.test(ncp = 2, null.ncp = 1, df1 = 1, df2 = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.107653633, ncp = 2, null.ncp = 1, df1 = 1, df2 = 100, alpha = 0.05, f.alpha = 7.19401376))
    expect_equal(power.f.test(ncp = 10, df1 = 2, df2 = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.8029764, ncp = 10, null.ncp = 0, df1 = 2, df2 = 100, alpha = 0.05, f.alpha = 3.0872959))
    expect_equal(power.f.test(power = 0.80, df1 = 4, df2 = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.800000048, ncp = 12.513776, null.ncp = 0, df1 = 4, df2 = 100, alpha = 0.05,
                      f.alpha = 2.462614926))
    expect_equal(power.f.test(power = 0.80, null.ncp = 0.1, df1 = 4, df2 = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 list(power = 0.799999968, ncp = 12.8338964, null.ncp = 0.1, df1 = 4, df2 = 100, alpha = 0.05,
                      f.alpha = 2.5237579))
    expect_error(power.f.test(ncp = 1, null.ncp = 11, df1 = 1, df2 = 100, alpha = 0.05, plot = FALSE, verbose = 0),
                 "`ncp` should be greater than or equal to `null.ncp`.")
    expect_error(power.f.test(power = 0.8, ncp = 1, df1 = 1, df2 = 100),
                 "Exactly one of the parameters `ncp` or `power` must be given, one has to be NULL.")
    expect_error(power.f.test(power = 0.8, ncp = NULL, df1 = 0.9, df2 = 100),
                 "`df1` can not be NULL, and needs to be at least 1.")
    expect_error(power.f.test(power = 0.8, ncp = NULL, df1 = 1, df2 = 2),
                 "`df2` can not be NULL, and needs to be at least 3.")
})
