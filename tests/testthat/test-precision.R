CIs <- c(0.80, 0.90, 0.95, 1 - (10 ^ seq(-2, -6)))
# precision.binom.test (= precision.binom) -----------------------------------------------------------------------------
test_that("precision.binom.test works", {
    expect_equal(vapply(c(0.001, 0.01, seq(0.1, 0.9, 0.1), 0.99, 0.999), function(p)
                   vapply(10 ^ seq(6), function(s) 
                     vapply(CIs, function(c)
                                   if (isInt(p * s))
                                     as.numeric(precision.binom(statistic = p, size = s, probs = ci.to.probs(c))$ci)
                                   else
                                     c(NA, NA),
                            numeric(2)),
                          numeric(16)),
                        numeric(96)),
                 vapply(c(0.001, 0.01, seq(0.1, 0.9, 0.1), 0.99, 0.999), function(p)
                   vapply(10 ^ seq(6), function(s) 
                     vapply(CIs, function(c)
                                   if (isInt(p * s))
                                     as.numeric(stats::binom.test(x = as.integer(p * s), n = s, conf.level = c)$conf.int)
                                   else
                                     c(NA, NA),
                            numeric(2)),
                          numeric(16)),
                        numeric(96)))

    expect_equal(power.binom.test(prob = 0.70, size = 100, verbose = 0, plot = FALSE) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.600185324, 0.7, 0.78759358),
                            power = c(0.463598612, 0.979011424, 0.999983617)))
})

# precision.chisq.test (= precision.chisq) -----------------------------------------------------------------------------
# code to create the comparison matrix (needs MBESS to be installed):
# cmpRes <- vapply(c(0.1, 0.5, 1, 2, 4, 6, 10, 15, 20, 30), function(s)
#             vapply(c(1, 2, 3, 5, 10, 20), function(d) 
#               vapply(CIs, function(c)
#                             as.numeric(MBESS::conf.limits.nc.chisq(s, df = d, conf.level = c)[c("Lower.Limit", "Upper.Limit")]),
#                      numeric(2)),
#                   numeric(16)),
#                  numeric(96))
# cmpRes[is.na(cmpRes)] <- 1e-12
# saveRDS(cmpRes, "precision.chisq_2compare.rds")  NB: store the file in `tests` under the main directory of the package
test_that("precision.chisq.test works", {
    expect_equal(vapply(c(0.1, 0.5, 1, 2, 4, 6, 10, 15, 20, 30), function(s)
                   vapply(c(1, 2, 3, 5, 10, 20), function(d) 
                     vapply(CIs, function(c)
                                   as.numeric(precision.chisq(statistic = s, df = d, probs = ci.to.probs(c))$ci),
                            numeric(2)),
                          numeric(16)),
                        numeric(96)),
                 readRDS("../precision.chisq_2compare.rds"), tolerance = 1e-4)

    expect_equal(power.chisq.test(ncp = 40, null.ncp = 10, df = 10, verbose = 0, plot = FALSE) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(11.630277918, 30.956990019, 58.299646843),
                            power = c(0.076270961, 0.697786973, 0.993279069)))
})

# precision.f.test (= precision.f) -------------------------------------------------------------------------------------
# code to create the comparison matrix (needs MBESS to be installed; NB: takes REALLY long!, reduced df = 20 -> 17.9):
# cmpRes <- vapply(c(0.1, 0.5, 1, 2, 3, 5, 10, 17.9), function(s)
#             vapply(c(1, 2, 4, 10, 30), function(d1)
#               vapply(c(2, 5, 10, 30, 100), function(d2)
#                 vapply(CIs, function(c)
#                               as.numeric(MBESS::conf.limits.ncf(F.value = s, df.1 = d1, df.2 = d2, conf.level = c)[c("Lower.Limit", "Upper.Limit")]),
#                        numeric(2)),
#                      numeric(2 * 8)),
#                    numeric(10 * 8)),
#                  numeric(50 * 8))
# cmpRes[is.na(cmpRes)] <- 0
# saveRDS(cmpRes, "precision.f_2compare.rds")  # NB: store the file in `tests` under the main directory of the package
test_that("precision.f.test works", {
    expect_equal(vapply(c(0.1, 0.5, 1, 2, 3, 5, 10, 17.9), function(s)
                   vapply(c(1, 2, 4, 10, 30), function(d1)
                     vapply(c(2, 5, 10, 30, 100), function(d2)
                       vapply(CIs, function(c)
                                     as.numeric(precision.f(statistic = s, df1 = d1, df2 = d2, probs = ci.to.probs(c))$ci),
                              numeric(2)),
                            numeric(2 * 8)),
                          numeric(10 * 8)),
                        numeric(50 * 8)),
                 readRDS("../precision.f_2compare.rds"), tolerance = 1e-4)

    expect_equal(power.f.regression(r.squared = 0.10, k.total = 3, n = 100, verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.287345930, 8.569135549, 25.270096289),
                            r.squared = c(0.002984254, 0.081947082, 0.208378628),
                            power = c(0.066717307, 0.667762835, 0.991778601)))
    expect_equal(power.f.regression(r.squared = 0.10, k.total = 3, power = 0.80, verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.4041219, 8.903946, 25.815163),
                            r.squared = c(0.004065444, 0.082517335, 0.206827141),
                            power = c(0.073871873, 0.686991399, 0.992919809)))
    expect_equal(power.f.ancova(eta.squared = 0.10, factor.levels = 4, n.total = 100, verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.287345930, 8.569135549, 25.270096289),
                            eta.squared = c(0.002984254, 0.081947082, 0.208378628),
                            power = c(0.066717307, 0.667762835, 0.991778601)))
    expect_equal(power.f.ancova(eta.squared = 0.05, factor.levels = 4, power = 0.50, verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0, 3.784230771932, 16.734527856405),
                            eta.squared = c(0, 0.032683473, 0.129992537),
                            power = c(0.05, 0.329655277, 0.936480960)))
    expect_equal(suppressWarnings(power.f.mixed.anova(eta.squared = 0.16388400, factor.levels = c(2, 3), effect = "between",
                                                      rho.within = NA, n.total = 600, verbose = 0)) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(76.88788, 117.110887, 165.791849),
                            eta.squared = c(0.113926894, 0.163766052, 0.217064177), power = c(1, 1, 1)))
    expect_equal(power.f.mixed.anova(eta.squared = 0.45506516, factor.levels = c(2, 3), effect = "within",
                                     rho.within = 0.632544, epsilon = 0.9950807, n.total = 600, verbose = 0) |>
                                     precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(7387.72518, 8108.71166, 8870.31736),
                            eta.squared = c(0.861256881, 0.872014356, 0.881703261), power = c(1, 1, 1)))
    expect_equal(power.f.mixed.anova(eta.squared = 0.03386145, factor.levels = c(2,3), effect = "interaction",
                                     rho.within = 0.632544, epsilon = 0.9950807, n.total = 600, verbose = 0) |>
                                     precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(266.544427, 339.402880, 421.093821),
                            eta.squared = c(0.18298316, 0.22190165, 0.26135248), power = c(1, 1, 1)))
})

# precision.lp.test (= precision.lp) -----------------------------------------------------------------------------------
test_that("precision.lp.test works", {
    expect_equal(vapply(c(-6, -3, -2, -1, 0, 1, 2, 3, 6), function(s)
                   vapply(c(1, 2, 3, 5, 10, 30), function(d)
                     vapply(CIs, function(c)
                                   as.numeric(precision.lp(statistic = s, df = d, probs = ci.to.probs(c))$ci),
                            numeric(2)),
                          numeric(16)),
                        numeric(96)),
                 vapply(c(-6, -3, -2, -1, 0, 1, 2, 3, 6), function(s)
                   vapply(c(1, 2, 3, 5, 10, 30), function(d)
                     vapply(CIs, function(c)
                                   as.numeric(precision.t(statistic = s, df = d, probs = ci.to.probs(c))$ci),
                            numeric(2)),
                          numeric(16)),
                        numeric(96)))

    expect_equal(power.lp.test(ncp = 1, df = 100, verbose = 0, plot = FALSE) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-0.96733053, 0.99749901, 2.96236048),
                            power = c(0.16218038, 0.16948231, 0.83495012)))
})

# precision.t.test (= precision.t) -------------------------------------------------------------------------------------
# code to create the comparison matrix (needs MBESS to be installed):
# cmpRes <- vapply(c(-6, -3, -2, -1, 0, 1, 2, 3, 6), function(s)
#             vapply(c(1, 2, 3, 5, 10, 30), function(d) 
#               vapply(CIs, function(c)
#                             as.numeric(MBESS::conf.limits.nct(s, df = d, conf.level = c)[c("Lower.Limit", "Upper.Limit")]),
#                      numeric(2)),
#                   numeric(16)),
#                  numeric(96))
# saveRDS(cmpRes, "precision.t_2compare.rds")  NB: store the file in `tests` under the main directory of the package
test_that("precision.t.test works", {
    expect_equal(vapply(c(-6, -3, -2, -1, 0, 1, 2, 3, 6), function(s)
                   vapply(c(1, 2, 3, 5, 10, 30), function(d)
                     vapply(CIs, function(c)
                                   as.numeric(precision.t(statistic = s, df = d, probs = ci.to.probs(c))$ci),
                            numeric(2)),
                          numeric(16)),
                        numeric(96)),
                 readRDS("../precision.t_2compare.rds"))

    expect_equal(power.t.test(ncp = NULL, df = 30, power = 0.50, verbose = 0, plot = FALSE) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-0.01620098, 2.00767839, 4.03434598),
                            t = c(-0.01620098, 2.00767839, 4.03434598), power = c(0.0500282, 0.49338062, 0.97394176)))
    expect_equal(power.t.test(ncp = NULL, null.ncp = c(-2, 2), df = 30, power = 0.50, alternative = "two.one.sided",
                              verbose = 0, plot = FALSE) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.9665903, 4.1864067, 6.4271167),
                            t = c(1.9665903, 4.1864067, 6.4271167), power = c(0.04330176, 0.48678743, 0.97275623)))
    expect_equal(power.t.student(d = 0.3536124, n.ratio = 0.50, n2 = 1000, design = "independent", claim.basis = "md.pval",
                                 verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(4.481438037, 6.454967319, 8.428534469),
                            d = c(0.24545847, 0.353553121, 0.461649846), power = c(0.994108867, 0.999996453, 1)))
    expect_equal(power.t.student(d = 0.50, n.ratio = 1, n2 = 64, design = "independent", claim.basis = "md.pval",
                                 verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.8322142, 2.8227631, 4.8137551),
                            d = c(0.147116083, 0.498998734, 0.85095972), power = c(0.131049958, 0.799889280, 0.997574822)))
    expect_equal(power.t.student(d = NULL, margin = c(-0.05, 0.05), power = 0.80, n2 = 200, alternative = "two.one.sided",
                                 design = "independent", verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.33680383, 3.3101839, 5.2836364),
                            d = c(0.133680383, 0.331018387, 0.528363638), power = c(NA_real_, NA_real_, NA_real_)))
    expect_equal(suppressWarnings(power.t.student(d = NULL, req.sign = 0, margin = c(-0.20, 0.20), power = 0.80, n2 = 1000,
                                                  alternative = "two.one.sided", design = "independent", verbose = 0)) |>
                                  precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-1.9599639845, 0, 1.959963985),
                            d = c(-0.087652254, 0, 0.0876522541), power = c(NA_real_, NA_real_, NA_real_)))
    expect_equal(power.t.student(d = -3.54, n2 = 10, design = "paired", claim.basis = "md.pval", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-16.604876435, -10.792003915, -5.720081120),
                            d = c(-5.2509230, -3.4127313, -1.8088485), power = c(1, 1, 0.9989675)))
    expect_equal(power.t.student(d = 0.3536124, n2 = 1000, design = "one.sample", claim.basis = "md.pval",
                                 verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(9.159215, 11.179353, 13.199913),
                            d = c(0.289639811, 0.353522182, 0.417417904), power = c(1, 1, 1)))
    expect_equal(power.t.welch(d = -0.3536124, var.ratio = 4, n.ratio = 0.50, n2 = 1000, claim.basis = "smd.ci",
                               verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-7.21340342, -5.23208461, -3.25088024),
                            d = c(-0.490401264, -0.353468376, -0.218691478), power = c(0.999999921, 0.999450914, 0.900766378)))
    expect_equal(power.np.wilcoxon(d = 0.50, n.ratio = 1, n2 = 64, design = "independent", method = "guenther",
                                   verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.7675896, 2.7581529, 4.7491704),
                            d = c(0.135691957, 0.487577163, 0.839542656), cles = c(0.538219343, 0.634866024, 0.723625648),
                            power = c(0.118612823, 0.781154984, 0.997028961)))
    expect_equal(power.t.reg(beta = 0.50, sd.predictor = 0.50, power = 0.80, r.squared = 0, verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.8322142, 2.8227631, 4.8137551),
                            beta = c(0.147116083, 0.498998734, 0.85095972), std.beta = c(0.07355804, 0.24949937, 0.42547986),
                            power = c(0.131049958, 0.79988928, 0.997574822)))
    expect_equal(power.t.reg(beta = 0.5133202, r.squared = 0.4884017, k.total = 2, n = 1e3, verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(20.4915723, 22.6885871, 24.8885946),
                            beta = c(0.463489425, 0.513182689, 0.562943643), std.beta = c(0.46348943, 0.51318269, 0.56294364),
                            power = c(1, 1, 1)))
    expect_equal(power.t.reg(beta = 0.3699354, sd.outcome = 7.169117, sd.predictor = 9.947825, r.squared = 0.4884017,
                             k.total = 2, n = 1e3, verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(20.4915723, 22.6885871, 24.8885946),
                            beta = c(0.334023765, 0.3698363, 0.405697617), std.beta = c(0.46348943, 0.51318270, 0.56294365),
                            power = c(1, 1, 1)))
})

# precision.z.test (= precision.z) -------------------------------------------------------------------------------------
test_that("precision.z.test works", {
    expect_equal(vapply(c(-6, -3, -2, -1, 0, 1, 2, 3, 6), function(s)
                   vapply(CIs, function(c)
                                 as.numeric(precision.z(statistic = s, probs = ci.to.probs(c))$ci),
                          numeric(2)),
                        numeric(16)),
                 vapply(c(-6, -3, -2, -1, 0, 1, 2, 3, 6), function(s)
                   vapply(CIs, function(c)
                                 s + qnorm(ci.to.probs(c)),
                          numeric(2)),
                        numeric(16)))

    expect_equal(unname(power.z.test(mean = NULL,          power = 0.50, plot = FALSE, verbose = 0) |> precision(verbose = 0)),
                 unname(power.t.test(ncp = NULL, df = 1e8, power = 0.50, plot = FALSE, verbose = 0) |> precision(verbose = 0)))
    expect_equal(power.z.test(mean = NULL,           power = 0.50, plot = FALSE, verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-0.000111064, 1.959852921, 3.919816905),
                            z = c(-0.000111064, 1.959852921, 3.919816905), power = c(0.05, 0.5, 0.9749935)))
    expect_equal(power.np.wilcoxon(d = 0.50, n.ratio = 1, n2 = 64, design = "independent", method = "noether",
                                   verbose = 0) |> precision(verbose = 0), 
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(0.7474706, 2.7074346, 4.6673986),
                            d = c(0.135424361, 0.5, 0.901916456), cles = c(0.5381442, 0.6381632, 0.7381822),
                            power = c(0.116052067, 0.772611768, 0.996609729)))
    expect_equal(power.z.oneprop(prob = 0.55, null.prob = c(0.40, 0.60), alternative = "two.one.sided", power = 0.80,
                                 std.error = "alternative", verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-1.959964, 0, 1.959964),
                            prop = c(0.510185651, 0.55, 0.589501558), h = c(-0.180985209, -0.1011905, -0.021384812),
                            power = c(NA_real_, NA_real_, NA_real_)))
    expect_equal(power.z.oneprop(prob = NULL, req.sign = 1, null.prob = c(0.40, 0.60), alternative = "two.one.sided",
                                 n = 1000, power = 0.80, std.error = "alternative", verbose = 0) |> precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-1.959964, 0, 1.959964),
                            prop = c(0.999899673, 0.999899984, 0.999900293), h = c(1.34940539, 1.34943643, 1.34946737),
                            power = c(NA_real_, NA_real_, NA_real_)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "normal", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(3.328446908, 5.288410892, 7.248374877),
                            beta = c(0.629385079, 1, 1.370614921), odds.ratio = c(1.87645635, 2.71828183, 3.93777137),
                            power = c(0.939208682, 0.9999934, 1)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "poisson", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.9667499, 3.92671388, 5.88667787),
                            beta = c(0.50086407, 1, 1.49913593), odds.ratio = c(1.650146497, 2.718281828, 4.477818250),
                            power = c(0.503220354, 0.998377017, 1)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "uniform", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-0.1775751, 1.7823889, 3.7423529),
                            beta = c(-0.099627567, 1, 2.099627567), odds.ratio = c(0.905174472, 2.718281828, 8.163129126),
                            power = c(0.05357613, 0.428201099, 0.97423447)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "exponential", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.2732960, 3.2332600, 5.1932240),
                            beta = c(0.393811823, 1, 1.606188177), odds.ratio = c(1.482621527, 2.718281828, 4.983777698),
                            power = c(0.195826933, 0.989302699, 1)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "binomial", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.06063668, 3.02060066, 4.98056465),
                            beta = c(0.351134359, 1, 1.648865641), odds.ratio = c(1.420678194, 2.718281828, 5.201076593),
                            power = c(0.184417184, 0.865667462, 0.999707289)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "bernoulli", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.06063668, 3.02060066, 4.98056465),
                            beta = c(0.351134359, 1, 1.648865641), odds.ratio = c(1.420678194, 2.718281828, 5.201076593),
                            power = c(0.184417184, 0.865667462, 0.999707289)))
    expect_equal(power.z.logistic(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "lognormal", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(1.2234986, 3.1834626, 5.1434266),
                            beta = c(0.38432951, 1, 1.61567049), odds.ratio = c(1.468629291, 2.718281828, 5.0312602),
                            power = c(0.127945103, 0.986432659, 0.999999999)))
    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "normal", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(21.35593664, 23.31590062, 25.27586461),
                            beta = c(0.915938740, 1, 1.084061260), rate.ratio = c(2.499120175, 2.718281828, 2.956662978),
                            power = c(1, 1, 1)))
    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "poisson", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(68.6962385, 70.6562025, 72.6161665),
                            beta = c(0.97226055, 1, 1.02773945), rate.ratio = c(2.643914418, 2.718281828, 2.794741028),
                            power = c(1, 1, 1)))
    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "uniform", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(4.7442012, 6.7041652, 8.6641292),
                            beta = c(0.7076498, 1, 1.2923502), rate.ratio = c(2.029216599, 2.718281828, 3.641334347),
                            power = c(0.997591739, 0.999999421, 1)))
    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "exponential", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(412.475115, 414.435079, 416.395043),
                            beta = c(0.995270758, 1, 1.004729242), rate.ratio = c(2.705456765, 2.718281828, 2.731167689),
                            power = c(1, 1, 1)))
    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "binomial", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(9.01870546, 10.97866945, 12.93863343),
                            beta = c(0.821475271, 1, 1.178524729), rate.ratio = c(2.273851913, 2.718281828, 3.249576658),
                            power = c(1, 1, 1)))
    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "bernoulli", verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(9.01870546, 10.97866945, 12.93863343),
                            beta = c(0.821475271, 1, 1.178524729), rate.ratio = c(2.273851913, 2.718281828, 3.249576658),
                            power = c(1, 1, 1)))
#    expect_equal(power.z.poisson(beta0 = 0.50, beta1 = 1, n = 200, alpha = 0.05, distribution = "lognormal", verbose = 0) |>
#                   precision(verbose = 0),
#                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(21.35593664, 23.31590062, 25.27586461),
#                            beta = c(0.915938740, 1, 1.084061260), rate.ratio = c(2.499120175, 2.718281828, 2.956662978),
#                            power = c(1, 1, 1)))
    expect_equal(power.z.mediation(beta.a = 0.25, sd.predictor = 0.50, beta.b = 0.25, beta.cp = 0.10, n = 200, verbose = 0) |>
                   precision(verbose = 0),
                 data.frame(probs = c(0.025, 0.5, 0.975), quantile = c(-0.3607055, 1.5992585, 3.5592225),
                            beta.indirect = c(-0.01409659, 0.0625, 0.13909659),
                            std.beta.indirect = c(-0.007048295, 0.03125, 0.069548295),
                            power = c(0.065033931, 0.359345798, 0.945118431)))
})

rm(CIs)
