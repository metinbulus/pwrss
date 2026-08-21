# lambdap -------------------------------------------------------------------------------------------------------------
test_that("lambdap distribution functions work", {
    # p's for very high quantiles (> +10) should be 1, and for very low quantiles (< -10) 0
    expect_equal(plambdap(q = seq(+10, +100, +10), df = 10, t = 2.5), rep(1, 10))
    expect_equal(plambdap(q = seq(-10, -100, -10), df = 10, t = 2.5), rep(0, 10))
    # adding the p's for the lower and the upper tail should result in 1
    expect_equal(plambdap(seq(10), df = 10, t = 2.5, lower.tail = TRUE) +
                 plambdap(seq(10), df = 10, t = 2.5, lower.tail = FALSE), rep(1, 10))

    # the area of dlambdap should be 1 (for t-values far enough away from th df's)
    # and all densities should be positive
    for (t in seq(0, 4)) {
        expect_equal(sum(dlambdap(seq(-5, 10, 0.01), df = 15, t = t)) * 0.01, 1, tolerance = 1e-2)
        expect_true(all(dlambdap(seq(-5, 10, 0.01), df = 15, t = t) >= 0))
    }

    # calculating quantiles and those back into probabilities should reconstruct the original values
    for (t in seq(-4, 4)) {
        expect_equal(plambdap(qlambdap(seq(0.1, 0.9, 0.1), df = 8, t = t), df = 8, t = t),
                     seq(0.1, 0.9, 0.1), tolerance = 1e-5)
    }

    # quantiles should (roughly) match up between generated / random samples and the theoretical quantiles
    set.seed(1)
    expect_true(all(abs(as.numeric(summary(rlambdap(500, df = 20, t = 3))[c(2, 3, 5)]) - qlambdap(c(0.25, 0.50, 0.75), df = 20, t = 3)) < 0.1))

    expect_equal(dlambdap(11.1, 9, 10), 0.129447060)
    expect_equal(plambdap(11.1, 9, 10), 0.713413419)
    expect_equal(qlambdap(0.01, 9, 10), 4.245346922)
    # the lower-tail-quantiles for 0.01 < p < 0.99 should be equal to the upper-tail-quantiles for 0.99 > p > 0.01
    expect_equal(rev(vapply(seq(0.01, 0.99, 0.01), qlambdap, numeric(1), 9, 10, lower.tail = TRUE)),
                     vapply(seq(0.01, 0.99, 0.01), qlambdap, numeric(1), 9, 10, lower.tail = FALSE))

    expect_equal(as.numeric(summary(rlambdap(100, 9, 4))),
                 c(0.8134377, 2.9166152, 3.8276935, 3.8368040, 4.8667765, 7.3351102))

    # converting a vector of p's into quantiles and back under different settings (varying df, t, and which tail)
    ps <- seq(0.01, 0.99, 0.01)
    for (df in 2 ^ seq(4)) {
        for (t in seq(-4, 4, 1)) {
            for (lt in c(TRUE, FALSE)) {
                qs <- suppressWarnings(vapply(ps, qlambdap, numeric(1), df = df, t = t, lower.tail = lt))
                expect_true(all(sign(diff(qs)) == ifelse(lt, 1, -1)))
                expect_equal(plambdap(qs, df = df, t = t, lower.tail = lt), ps, tolerance = 1e-5)
            }
        }
    }

    # when generating a random vector of lambda-prime values and afterwards calculating quantiles for this vector,
    # the proportion of quantiles equal or larger the ncp used for creating the random vector should be (roughly)
    # equal to p
    set.seed(23)
    true.ncp <- 3
    ts <- rlambdap(1000, 128, true.ncp)
    for (p in c(0.05, 0.25, 0.50, 0.75, 0.95)) {
        expect_equal(mean(qlambdap(p, df = 128, ts) >= true.ncp), p, tolerance = 10 ^ -1.6)
    }

    expect_true(Inf  == qlambdap(1, df, 1, lower.tail = TRUE))
    expect_true(-Inf == qlambdap(1, df, 1, lower.tail = FALSE))
    expect_true(-Inf == qlambdap(0, df, 1, lower.tail = TRUE))
    expect_true(Inf  == qlambdap(0, df, 1, lower.tail = FALSE))

    expect_true(1 == plambdap(+Inf, df, 1, lower.tail = TRUE))
    expect_true(1 == plambdap(-Inf, df, 1, lower.tail = FALSE))
    expect_true(0 == plambdap(-Inf, df, 1, lower.tail = TRUE))
    expect_true(0 == plambdap(+Inf, df, 1, lower.tail = FALSE))
})
