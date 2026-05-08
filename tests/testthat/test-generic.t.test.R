test_that("generic.t.test.R works", {
    # power.t.test -----------------------------------------------------------------------------------------------------
    expect_equal(power.t.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", ncp = 1.96, null.ncp = 0, df = 100, alpha = 0.05, t.alpha = 1.98397152 * c(-1, 1),
                      beta = 0.50745274, type.s = 0.0000970993, type.m = 1.427979969, power = 0.49254726))

    expect_equal(power.t.test(ncp = 1.96, df = 120, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "one.sided", ncp = 1.96, null.ncp = 0, df = 120, alpha = 0.05, t.alpha = 1.6576509,
                      beta = 0.38053400, type.s = 0, type.m = NA, power = 0.619466))

    expect_equal(power.t.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.35551537 * c(-1, 1), beta = 0.722952351, type.s = NA, type.m = NA, power = 0.277047649))

    expect_equal(power.t.test(ncp = 0, null.ncp = 2, df = 100, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.35551537 * c(-1, 1), beta = 0.722952351, type.s = NA, type.m = NA, power = 0.277047649))

    expect_equal(power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 400, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 2, null.ncp = c(-1, 1), df = 400, alpha = 0.05,
                      t.alpha = 2.97258983 * c(-1, 1), beta = 0.832831958, type.s = NA, type.m = NA, power = 0.167168042))

    expect_equal(power.t.test(ncp = 2, null.ncp = -1, df = 400, alpha = 0.05, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.one.sided", ncp = 2, null.ncp = c(-1, 1), df = 400, alpha = 0.05,
                      t.alpha = 2.97258983 * c(-1, 1), beta = 0.832831958, type.s = NA, type.m = NA, power = 0.167168042))

    # example 25.3 from the GPower manual
    expect_equal(power.t.test(ncp = -1.25, null.ncp = 0, df = 24, alpha = 0.05, alternative = "two.sided", plot = FALSE, verbose = 0),
                 list(alternative = "two.sided", ncp = -1.25, null.ncp = 0, df = 24, alpha = 0.05, t.alpha = 2.063898562 * c(-1, 1),
                      beta = 0.775475958, type.s = 0.0034900921, type.m = 2.1835685, power = 0.22452404))
    # result is identical: power ~ 0.224524/5

    expect_error(power.t.test(ncp = 2, null.ncp = 0, df = c(1, 1), alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "`df` must be numeric, have a value of at least 1 and have a length of 1.")
    expect_error(power.t.test(ncp = 2, null.ncp = 0, df = 0, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "`df` must be numeric, have a value of at least 1 and have a length of 1.")
    expect_error(power.t.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.ncp` must be of length one.")
    expect_error(power.t.test(ncp = 2, null.ncp = c(-1, 1, 1), df = 100, alpha = 0.05, alternative = "two.one.sided",
                              plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.ncp` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds\\)."))
    expect_error(power.t.test(ncp = 2, alpha = 0.05, df = 100, alternative = "two.one.sided", plot = FALSE, verbose = 0),
                 paste("If `alternative` is \"two.one.sided\", `null.ncp` must be of length one \\(absolute value,",
                       "that must be different from 0\\), or length two \\(lower and upper bounds\\)."))

    # ncp.t.test -------------------------------------------------------------------------------------------------------
    expect_equal(ncp.t.test(power = 0.80, df = 100, alpha = 0.05, alternative = "two.sided"),
                 list(alternative = "two.sided", ncp = 2.82885649, null.ncp = 0, df = 100, alpha = 0.05,
                      t.alpha = 1.98397152 * c(-1, 1), beta = 0.199997428, type.s = 0.00000119770848, type.m = 1.1334503,
                      power = 0.800002572))
    expect_equal(ncp.t.test(power = 0.80, df = 100, alpha = 0.05, alternative = "one.sided"),
                 list(alternative = "one.sided", ncp = 2.50346543, null.ncp = 0, df = 100, alpha = 0.05,
                      t.alpha = 1.66023433, beta = 0.200001263, type.s = 0, type.m = NA, power = 0.799998737))
    expect_equal(ncp.t.test(power = 0.80, req.sign = "0", null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                            alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
                      t.alpha = 0.35551537 * c(-1, 1), beta = 0.72295235, type.s = NA, type.m = NA, power = 0.27704765))
    expect_equal(ncp.t.test(power = 0.80, req.sign = "+", null.ncp = c(-1, 1), df = 100, alpha = 0.05, 
                            alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = 3.8638626, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
                      t.alpha = 3.0109934 * c(-1, 1), beta = 0.200005339, type.s = NA, type.m = NA, power = 0.799994661))

    expect_equal(ncp.t.test(power = 0.80, ncp = 3, alpha = 0.05, alternative = "two.sided"),
                 list(alternative = "two.sided", ncp = 3, null.ncp = 0, df = 14.8520054, alpha = 0.05,
                      t.alpha = 2.1333009 * c(-1, 1), beta = 0.199999989, type.s = 0.00000106692622,
                      type.m = 1.186699976, power = 0.8))
    expect_equal(ncp.t.test(power = 0.80, ncp = 3, alpha = 0.05, alternative = "one.sided"),
                 list(alternative = "one.sided", ncp = 3, null.ncp = 0, df = 4.2402269, alpha = 0.05,
                      t.alpha = 2.09761361, beta = 0.199999338, type.s = 0, type.m = NA, power = 0.800000662))
    expect_equal(ncp.t.test(power = 0.80, ncp = 0, null.ncp = c(-2, 2), alpha = 0.05,
                            alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = 0, null.ncp = c(-2, 2), df = 9955311578.2, alpha = 0.05,
                      t.alpha = 0.355146373 * c(-1, 1), beta = 0.722479937, type.s = NA, type.m = NA,
                      power = 0.277520063))
    expect_equal(ncp.t.test(power = 0.80, ncp = 4, null.ncp = c(-1, 1), alpha = 0.05, 
                            alternative = "two.one.sided"),
                 list(alternative = "two.one.sided", ncp = 4, null.ncp = c(-1, 1), df = 32.409657, alpha = 0.05,
                      t.alpha = 3.12206879 * c(-1, 1), beta = 0.199999993, type.s = NA, type.m = NA, power = 0.8))
    expect_error(ncp.t.test(power = 0.80, alpha = 0.05, alternative = "two.sided"),
                 "Exactly one of the parameters `ncp` or `df` must be given, one has to be NULL.")
})
