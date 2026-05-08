test_that("generic.z.test.R works", {
    # power.z.test -----------------------------------------------------------------------------------------------------
    expect_equal(power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", mean = 1.96, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.959964 * c(-1, 1), beta = 0.49994135, type.s = 0.000088551816, type.m = 1.40704658, power = 0.500058649))
    expect_equal(power.z.test(mean = 1.96, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "one.sided", mean = 1.96, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.64485363, beta = 0.37632526, type.s = 0, type.m = NA, power = 0.62367474))
    expect_equal(power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 0, sd = 1, null.mean = c(-2, 2), null.sd = 1, alpha = 0.05,
                      z.alpha = 0.355146373 * c(-1, 1), beta = 0.72247994, type.s = NA, type.m = NA, power = 0.277520063))
    expect_equal(power.z.test(mean = 0, null.mean = 2, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 0, sd = 1, null.mean = c(-2, 2), null.sd = 1, alpha = 0.05,
                      z.alpha = 0.355146373 * c(-1, 1), beta = 0.72247994, type.s = NA, type.m = NA, power = 0.277520063))
    expect_equal(power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 2, sd = 1, null.mean = c(-1, 1), null.sd = 1, alpha = 0.05,
                      z.alpha = 2.959964 * c(-1, 1), beta = 0.83146298, type.s = NA, type.m = NA, power = 0.168537023))
    expect_equal(power.z.test(mean = 2, null.mean = -1, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", mean = 2, sd = 1, null.mean = c(-1, 1), null.sd = 1, alpha = 0.05,
                      z.alpha = 2.959964 * c(-1, 1), beta = 0.83146298, type.s = NA, type.m = NA, power = 0.168537023))
    expect_error(power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.mean` must be of length one.")
    expect_error(power.z.test(mean = 2, null.mean = c(-1, 1, 1), alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.mean` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds)."))
    expect_error(power.z.test(mean = 2, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.mean` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds\\)."))

    # ncp.z.test -------------------------------------------------------------------------------------------------------
    expect_equal(ncp.z.test(power = 0.80, alpha = 0.05, alternative = "two.sided"),
                 list(alternative = "two.sided", mean = 2.8015654, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.959964 * c(-1, 1), beta = 0.200004586, type.s = 0.00000120082756,
                      type.m = 1.12491569, power = 0.799995414))
    expect_equal(ncp.z.test(power = 0.80, alpha = 0.05, alternative = "one.sided"),
                 list(alternative = "one.sided", mean = 2.4864815, sd = 1, null.mean = 0, null.sd = 1, alpha = 0.05,
                      z.alpha = 1.644853627, beta = 0.199998146, type.s = 0, type.m = NA, power = 0.800001854))
    expect_equal(ncp.z.test(power = 0.80, req.sign = "0", null.mean = c(-2, 2), alpha = 0.05, alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", mean = 0, sd = 1, null.mean = c(-2, 2), null.sd = 1, alpha = 0.05,
                      z.alpha = 0.355146373 * c(-1, 1), beta = 0.722479937, type.s = NA, type.m = NA, power = 0.277520063))
    expect_equal(ncp.z.test(power = 0.80, req.sign = "+", null.mean = c(-1, 1), alpha = 0.05, alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", mean = 3.8015688, sd = 1, null.mean = c(-1, 1), null.sd = 1,
                      alpha = 0.05, z.alpha = 2.959964 * c(-1, 1), beta = 0.200004607, type.s = NA, type.m = NA,
                      power = 0.79999539))
    expect_equal(ncp.z.test(power = 0.80, req.sign = "-", null.mean = c(-1, 1), alpha = 0.05, alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", mean = -3.8015688, sd = 1, null.mean = c(-1, 1), null.sd = 1,
                      alpha = 0.05, z.alpha = 2.959964 * c(-1, 1), beta = 0.200004607, type.s = NA, type.m = NA,
                      power = 0.79999539))
    expect_error(ncp.z.test(mean = 1), "`mean` needs to be NULL.")
})
