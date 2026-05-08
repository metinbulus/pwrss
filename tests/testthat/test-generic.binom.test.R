test_that("generic.binom.test.R works", {
    # power.binom.test -------------------------------------------------------------------------------------------------
    expect_equal(power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", plot = FALSE,
                                  verbose = 0),
                 list(size = 200, alpha = 0.038418816, alternative = "one.sided", prob = 0.6, null.prob = 0.5,
                              binom.alpha = 112, power = 0.860335667))
    expect_equal(power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05, alternative = "two.sided",
                                  plot = FALSE, verbose = 0),
                 list(size = 200, alpha = 0.040037192, alternative = "two.sided", prob = 0.4, null.prob = 0.5,
                      binom.alpha = c(85, 114), power = 0.786848265))
    expect_equal(power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided",
                                  plot = FALSE, verbose = 0),
                 list(size = 200, alpha = 0.049184711, alternative = "two.one.sided", prob = 0.5, null.prob = c(0.4, 0.6),
                      binom.alpha = c(91, 108), power = 0.7707534))
    expect_equal(power.binom.test(size = 200, prob = 0.7, null.prob = c(0.4, 0.6), alpha = 0.05, alternative = "two.one.sided",
                                  plot = FALSE, verbose = 0),
                 list(size = 200, alpha = 0.049432239, alternative = "two.one.sided", prob = 0.7, null.prob = c(0.4, 0.6),
                      binom.alpha = c(66, 133), power = 0.84208844))
    expect_error(power.binom.test(size = 200, prob = 0.6, null.prob = c(0.5, 0.7), alpha = 0.05, alternative = "one.sided",
                                  plot = FALSE, verbose = 0),
                 "If `alternative` is \"two.sided\" or \"one.sided\", `null.prob` must be of length one.")
    expect_error(power.binom.test(size = -1, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "one.sided", plot = FALSE, verbose = 0),
                 "Argument `size` does not have a valid value \\(integer-like, >= 0, and finite\\).")
    expect_error(power.binom.test(size = 2, prob = 0.6, null.prob = 0.5, alpha = 0.05, alternative = "two.one.sided", plot = FALSE),
                 paste("If `alternative` is \"two.one.sided\", `null.prob` must be of length two \\(lower and upper bounds,",
                       "with the upper bound being larger than the lower bound\\)."))
    expect_error(power.binom.test(size = 2, prob = 0.6, null.prob = c(0.5, 0.3), alpha = 0.05, alternative = "two.one.sided", plot = FALSE),
                 paste("If `alternative` is \"two.one.sided\", `null.prob` must be of length two \\(lower and upper bounds,",
                       "with the upper bound being larger than the lower bound\\)."))

    # ncp.binom.test ---------------------------------------------------------------------------------------------------
    expect_equal(prob.binom.test(size = 200, power = 0.80, null.prob = 0.5, alpha = 0.05, alternative = "two.sided"),
                 list(size = 200, alpha = 0.040037192, alternative = "two.sided", prob = 0.601607788, null.prob = 0.5,
                      binom.alpha = c(85, 114), power = 0.80015963))
    expect_equal(prob.binom.test(size = 200, power = 0.80, null.prob = 0.5, alpha = 0.05, alternative = "one.sided"),
                 list(size = 200, alpha = 0.038418816, alternative = "one.sided", prob = 0.591701527, null.prob = 0.5,
                      binom.alpha = 112, power = 0.79993916))
    expect_equal(prob.binom.test(size = 200, req.sign = "0", power = 0.80, null.prob = c(0.4, 0.6), alpha = 0.05,
                                 alternative = "two.one.sided"),
                 list(size = 200, alpha = 0.049184711, alternative = "two.one.sided", prob = 0.5, null.prob = c(0.4, 0.6),
                      binom.alpha = c(91, 108), power = 0.7707534))
    expect_equal(prob.binom.test(size = 200, req.sign = "+", power = 0.80, null.prob = c(0.4, 0.6), alpha = 0.05,
                                 alternative = "two.one.sided"),
                 list(size = 200, alpha = 0.049432239, alternative = "two.one.sided", prob = 0.694802783, null.prob = c(0.4, 0.6),
                      binom.alpha = c(66, 133), power = 0.79994722))
    expect_equal(prob.binom.test(size = 200, req.sign = "-", power = 0.80, null.prob = c(0.4, 0.6), alpha = 0.05,
                                 alternative = "two.one.sided"),
                 list(size = 200, alpha = 0.049432239, alternative = "two.one.sided", prob = 0.305197217, null.prob = c(0.4, 0.6),
                      binom.alpha = c(66, 133), power = 0.79994722))
    expect_error(prob.binom.test(size = NULL),            "`size` can not be NULL, and prob` needs to be NULL.")
    expect_error(prob.binom.test(size = 200, prob = 0.5), "`size` can not be NULL, and prob` needs to be NULL.")
})
