# lambdap -------------------------------------------------------------------------------------------------------------
test_that("lambdap distribution functions work", {
    # border cases for qlambdap and plambdap
    expect_identical(qlambdap(1,    4, 1, lower.tail = TRUE,  log.p = FALSE),  Inf)
    expect_identical(qlambdap(1,    4, 1, lower.tail = FALSE, log.p = FALSE), -Inf)
    expect_identical(qlambdap(0,    4, 1, lower.tail = TRUE,  log.p = FALSE), -Inf)
    expect_identical(qlambdap(0,    4, 1, lower.tail = FALSE, log.p = FALSE),  Inf)
    expect_identical(qlambdap(2,    4, 1, lower.tail = FALSE, log.p = FALSE),  NaN)
    expect_identical(qlambdap(0,    4, 1, lower.tail = TRUE,  log.p = TRUE),   Inf)
    expect_identical(qlambdap(0,    4, 1, lower.tail = FALSE, log.p = TRUE),  -Inf)
    expect_identical(qlambdap(-Inf, 4, 1, lower.tail = TRUE,  log.p = TRUE),  -Inf)
    expect_identical(qlambdap(-Inf, 4, 1, lower.tail = FALSE, log.p = TRUE),   Inf)
    expect_identical(qlambdap(1,    4, 1, lower.tail = TRUE,  log.p = TRUE),   NaN)
    expect_identical(plambdap(+Inf, 4, 1, lower.tail = TRUE),  1)
    expect_identical(plambdap(-Inf, 4, 1, lower.tail = FALSE), 1)
    expect_identical(plambdap(-Inf, 4, 1, lower.tail = TRUE),  0)
    expect_identical(plambdap(+Inf, 4, 1, lower.tail = FALSE), 0)

    # check correct handling invalid df's (<= 0 or NaN)
    expect_identical(dlambdap(0, df = 0,   ncp = 0), NaN)
    expect_identical(dlambdap(0, df = -1,  ncp = 0), NaN)
    expect_identical(dlambdap(0, df = NaN, ncp = 0), NaN)
    expect_identical(plambdap(0, df = 0,   ncp = 0), NaN)
    expect_identical(plambdap(0, df = -1,  ncp = 0), NaN)
    expect_identical(plambdap(0, df = NaN, ncp = 0), NaN)
    expect_identical(qlambdap(0, df = 0,   ncp = 0), NaN)
    expect_identical(plambdap(0, df = -1,  ncp = 0), NaN)
    expect_identical(qlambdap(0, df = NaN, ncp = 0), NaN)

    # correct handling of n in rlambdap
    expect_length(rlambdap(runif(20),  df = 20, ncp = 0), 20)
    expect_length(rlambdap(0,          df = 20, ncp = 0),  0)
    expect_length(rlambdap(NULL,       df = 20, ncp = 0),  0)
    expect_length(rlambdap(numeric(0), df = 20, ncp = 0),  0)

    # log = TRUE / FALSE reveal the same results in dlambdap
    xs <- as.numeric(seq(-10, 10))
    expect_equal(log(dlambdap(xs, df = 10, ncp = 2)), dlambdap(xs, df = 10, ncp = 2, log = TRUE))

    # p's for very high quantiles (> +10) should be 1, and for very low quantiles (< -10) 0
    expect_equal(plambdap(q = seq(+10, +100, +10), df = 10, ncp = 2.5), rep(1, 10))
    expect_equal(plambdap(q = seq(-10, -100, -10), df = 10, ncp = 2.5), rep(0, 10))
    # adding the p's for the lower and the upper tail should result in 1
    expect_equal(plambdap(seq(10), df = 10, ncp = 2.5, lower.tail = TRUE) +
                 plambdap(seq(10), df = 10, ncp = 2.5, lower.tail = FALSE), rep(1, 10))

    # the area of dlambdap should be 1 (for t-values far enough away from th df's)
    # and all densities should be positive
    for (ncp in seq(0, 4)) {
        expect_equal(sum(dlambdap(seq(-5, 10, 0.01), df = 15, ncp = ncp)) * 0.01, 1, tolerance = 1e-2)
        expect_true(all(dlambdap(seq(-5, 10, 0.01),  df = 15, ncp = ncp) >= 0))
    }

    # calculating quantiles and those back into probabilities should reconstruct the original values
    for (ncp in seq(-4, 4)) {
        expect_equal(plambdap(qlambdap(seq(0.1, 0.9, 0.1), df = 8, ncp = ncp), df = 8, ncp = ncp),
                     seq(0.1, 0.9, 0.1), tolerance = 1e-5)
    }

    # quantiles should (roughly) match up between generated / random samples and the theoretical quantiles
    set.seed(1)
    expect_true(all(abs(as.numeric(summary(rlambdap(500, df = 20, ncp = 3))[c(2, 3, 5)]) - qlambdap(c(0.25, 0.50, 0.75), df = 20, ncp = 3)) < 0.1))

    expect_equal(dlambdap(11.1, 9, 10), 0.129447059)
    expect_equal(plambdap(11.1, 9, 10), 0.713413419)
    expect_equal(qlambdap(0.01, 9, 10), 4.245346922)
    # the lower-tail-quantiles for 0.01 < p < 0.99 should be equal to the upper-tail-quantiles for 0.99 > p > 0.01
    expect_equal(rev(vapply(seq(0.01, 0.99, 0.01), qlambdap, numeric(1), 9, 10, lower.tail = TRUE)),
                     vapply(seq(0.01, 0.99, 0.01), qlambdap, numeric(1), 9, 10, lower.tail = FALSE))

    expect_equal(as.numeric(summary(rlambdap(100, 9, 4))),
                 c(0.8134377, 2.9166152, 3.8276935, 3.8368040, 4.8667765, 7.3351102))

    # converting a vector of p's into quantiles and back under different settings (varying df, ncp, and which tail)
    ps <- seq(0.01, 0.99, 0.01)
    lps <- log(ps)
    for (df in 2 ^ seq(4)) {
        for (ncp in seq(-4, 4, 1)) {
            for (lt in c(TRUE, FALSE)) {
                qs <- vapply(ps, qlambdap, numeric(1), df = df, ncp = ncp, lower.tail = lt)
                expect_true(all(sign(diff(qs)) == ifelse(lt, 1, -1)))
                expect_equal(plambdap(qs, df = df, ncp = ncp, lower.tail = lt), ps, tolerance = 1e-5)
                expect_equal(qs, suppressWarnings(vapply(lps, qlambdap, numeric(1), df = df, ncp = ncp, lower.tail = lt, log = TRUE)))
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

    # compare the values obtained with the lambdap implementation in pwrss to values to the calculation of confidence
    # intervals for the non-central t distribution in MBESS and effectsize
    # NB: If you would like to confirm the code, 'MBESS' and 'effectsize' need to be installed
    # NB: Results from MBESS were stored in lambda_CI.rds used below
    # ------------------------------------------------------------------------------------------------------------------
    # create functions to calculate CI's with our own implementation (f_ci_p) vs. MBESS (f_ci_s) and effectsize (f_ci_e)
    #f_ci_p <- function(df, ncp, alpha) qlambdap(p = c(alpha / 2, 1 - alpha / 2), df = df, ncp = ncp)
    #f_ci_m <- function(df, ncp, alpha) as.numeric(MBESS::conf.limits.nct(ncp = ncp, df = df, conf.level = 1 - alpha)[c("Lower.Limit", "Upper.Limit")])
    #f_ci_e <- function(df, ncp, alpha) effectsize:::.get_ncp_t(t = ncp, df_error = df, conf.level = 1 - alpha)
    ## define vectors with alphas and df's of interest  
    #s_alpha <- c(0.10, 0.05, 0.01, 0.001)
    #s_df <- unique(round(exp(seq(log(2), log(30), length.out = 20))))
    #res_ci_p <- res_ci_m <- res_ci_e <- array(NaN, c(length(s_df), 11, length(s_alpha), 2))
    #for (i in seq_along(s_df)) {
    #    l_ncp <- if (s_df[i] == 2) 10 else if (s_df[i] >= 3 && s_df[i] <= 7) 15 else 20
    #    s_ncp <- seq(-l_ncp, l_ncp, length.out = 11)
    #    for (j in seq_along(s_ncp)) {
    #        for (k in seq_along(s_alpha)) {
    #            res_ci_p[i, j, k, ] <- f_ci_p(df = s_df[i], ncp = s_ncp[j], alpha = s_alpha[k])
    #            res_ci_m[i, j, k, ] <- f_ci_m(df = s_df[i], ncp = s_ncp[j], alpha = s_alpha[k])
    #            res_ci_e[i, j, k, ] <- f_ci_e(df = s_df[i], ncp = s_ncp[j], alpha = s_alpha[k])
    #        }
    #    }
    #    cat(".")
    #}
    # ------------------------------------------------------------------------------------------------------------------
    res_ci <- readRDS("../lambdap_CI.rds")
    s_df <- unique(round(exp(seq(log(2), log(30), length.out = 20)))) # providing good coverage of possible df's
    s_alpha <- c(0.10, 0.05, 0.01, 0.001) # relect often used CI's (90, 95, 99, 99.9%)
    for (i in seq_along(s_df)) {
        l_ncp <- if (s_df[i] == 2) 10 else if (s_df[i] >= 3 && s_df[i] <= 7) 15 else 20
        s_ncp <- seq(-l_ncp, l_ncp, length.out = 11) # adjusted dynamically depending on the number of df's
        for (j in seq_along(s_ncp)) {
            for (k in seq_along(s_alpha)) {
                expect_equal(qlambdap(p = c(s_alpha[k] / 2, 1 - s_alpha[k] / 2), ncp = s_ncp[j], df = s_df[i]), res_ci[i, j, k, ])
            }
        }
    }    
})
