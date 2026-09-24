# Package index

## Generic Power Analyses

Power analyses based on core distributions

- [`power.binom.test()`](https://metinbulus.github.io/pwrss/reference/power.binom.test.md)
  : Power Analysis for the Generic Binomial Test
- [`power.chisq.test()`](https://metinbulus.github.io/pwrss/reference/power.chisq.test.md)
  : Statistical Power for the Generic Chi-Square Test
- [`power.f.test()`](https://metinbulus.github.io/pwrss/reference/power.f.test.md)
  : Statistical Power for the Generic F-Test
- [`power.lp.test()`](https://metinbulus.github.io/pwrss/reference/power.lp.test.md)
  : Statistical Power for the Lambda-Prime Distribution
- [`power.t.test()`](https://metinbulus.github.io/pwrss/reference/power.t.test.md)
  : Statistical Power for the Generic t-Test
- [`power.z.test()`](https://metinbulus.github.io/pwrss/reference/power.z.test.md)
  : Statistical Power for the Generic z-Test

## Proportions

Power analyses for one and two sample (independent and paired)
proportions

- [`power.chisq.gof()`](https://metinbulus.github.io/pwrss/reference/power.chisq.gof.md)
  : Power Analysis for Chi-square Goodness-of-Fit or Independence Tests
- [`power.z.oneprop()`](https://metinbulus.github.io/pwrss/reference/power.z.oneprop.md)
  : Power Analysis for the Test of One Proportion (Normal Approximation
  Method)
- [`power.exact.oneprop()`](https://metinbulus.github.io/pwrss/reference/power.exact.oneprop.md)
  : Power Analysis for the Test of One Proportion (Exact Method)
- [`power.z.twoprops()`](https://metinbulus.github.io/pwrss/reference/power.z.twoprops.md)
  : Power Analysis for Testing the Difference Between Two Proportions
  (Normal Approximation Method)
- [`power.exact.twoprops()`](https://metinbulus.github.io/pwrss/reference/power.exact.twoprops.md)
  : Power Analysis for Testing the Difference Between Two Proportions
  (Exact Method)
- [`power.exact.fisher()`](https://metinbulus.github.io/pwrss/reference/power.exact.fisher.md)
  : Power Analysis for Fisher's Exact Test (Independent Proportions)
- [`power.exact.mcnemar()`](https://metinbulus.github.io/pwrss/reference/power.exact.mcnemar.md)
  : Power Analysis for McNemar's Exact Test (Paired Proportions)

## Correlations

Power analyses for (independent and dependent) correlations

- [`power.z.onecor()`](https://metinbulus.github.io/pwrss/reference/power.z.onecor.md)
  : Power Analysis for One-Sample Correlation
- [`power.exact.onecor()`](https://metinbulus.github.io/pwrss/reference/power.exact.onecor.md)
  : Power Analysis for One-Sample Correlation (Exact)
- [`power.z.twocors()`](https://metinbulus.github.io/pwrss/reference/power.z.twocors.md)
  : Power Analysis for Independent Correlations
- [`power.z.twocors.steiger()`](https://metinbulus.github.io/pwrss/reference/power.z.twocors.steiger.md)
  : Power Analysis for Dependent Correlations (Steiger's Z-Test)

## Means Differences

Power analyses for mean differences (t-tests, Wilcoxon) and ANOVA /
ANCOVA models

- [`power.t.student()`](https://metinbulus.github.io/pwrss/reference/power.t.student.md)
  : Power Analysis for Student's t-Test
- [`power.t.welch()`](https://metinbulus.github.io/pwrss/reference/power.t.welch.md)
  : Power Analysis for Welch's t-Test
- [`power.np.wilcoxon()`](https://metinbulus.github.io/pwrss/reference/power.np.wilcoxon.md)
  : Power Analysis for Non-parametric Rank-Based Tests (One-Sample,
  Independent, and Paired Designs)
- [`power.f.ancova()`](https://metinbulus.github.io/pwrss/reference/power.f.ancova.md)
  : Power Analysis for One-, Two-, Three-Way ANOVA/ANCOVA Using Effect
  Size (F-Test)
- [`power.f.ancova.keppel()`](https://metinbulus.github.io/pwrss/reference/power.f.ancova.keppel.md)
  : Power Analysis for One-Way ANOVA/ANCOVA Using Means and Standard
  Deviations (F test)
- [`power.f.ancova.shieh()`](https://metinbulus.github.io/pwrss/reference/power.f.ancova.shieh.md)
  : Power Analysis for One-, Two-, Three-Way ANCOVA Using Means,
  Standard Deviations, and (Optionally) Contrasts (F test)
- [`factorial.contrasts()`](https://metinbulus.github.io/pwrss/reference/factorial.contrasts.md)
  : Factorial Contrasts
- [`power.t.contrast()`](https://metinbulus.github.io/pwrss/reference/power.t.contrast.md)
  : Power Analysis for One-, Two-, Three-Way ANCOVA Contrasts and
  Multiple Comparisons (T-Tests)
- [`power.t.contrasts()`](https://metinbulus.github.io/pwrss/reference/power.t.contrasts.md)
  : Power Analysis for One-, Two-, Three-Way ANCOVA Contrasts and
  Multiple Comparisons (T-Tests)
- [`power.f.mixed.anova()`](https://metinbulus.github.io/pwrss/reference/power.f.mixed.anova.md)
  : Power Analysis for Mixed-Effects Analysis of Variance (F-Test)

## Regression Models

Power analyses for linear, logistic, and Poisson regressions

- [`power.f.regression()`](https://metinbulus.github.io/pwrss/reference/power.f.regression.md)
  : Power Analysis for Linear Regression: R-squared or R-squared Change
  (F-Test)
- [`power.t.regression()`](https://metinbulus.github.io/pwrss/reference/power.t.regression.md)
  : Power Analysis for Linear Regression: Single Coefficient (T-Test)
- [`power.z.mediation()`](https://metinbulus.github.io/pwrss/reference/power.z.mediation.md)
  : Power Analysis for Indirect Effects in a Mediation Model (Z, Joint,
  and Monte Carlo Tests)
- [`power.z.logistic()`](https://metinbulus.github.io/pwrss/reference/power.z.logistic.md)
  : Power Analysis for Logistic Regression Coefficient (Wald's Z-Test)
- [`power.z.poisson()`](https://metinbulus.github.io/pwrss/reference/power.z.poisson.md)
  : Power Analysis for Poisson Regression Coefficient (Wald's z Test)

## Helper and Effect Size Conversions

Helper functions, and functions to transform and convert effect size
metrics

- [`inflate.sample()`](https://metinbulus.github.io/pwrss/reference/inflate.sample.md)
  : Inflate Sample Size for Attrition
- [`d.to.cles()`](https://metinbulus.github.io/pwrss/reference/d.to.cles.md)
  : Conversion from Cohen's d to Common Language Effect Size
- [`cor.to.z()`](https://metinbulus.github.io/pwrss/reference/cor.to.z.md)
  : Conversion from a correlation to a z-value (Fisher's
  z-transformation)
- [`cors.to.q()`](https://metinbulus.github.io/pwrss/reference/cors.to.q.md)
  : Conversion from a correlation Difference to Cohen's q
- [`etasq.to.f()`](https://metinbulus.github.io/pwrss/reference/etasq.to.f.md)
  : Conversion from Eta-squared to Cohen's f
- [`f.to.etasq()`](https://metinbulus.github.io/pwrss/reference/f.to.etasq.md)
  : Conversion between Cohen's f and Eta-squared
- [`f.to.rsq()`](https://metinbulus.github.io/pwrss/reference/f.to.rsq.md)
  : Conversion from Cohen's f to R-squared
- [`joint.probs.2x2()`](https://metinbulus.github.io/pwrss/reference/joint.probs.2x2.md)
  : Conversion from joint probabilities to marginal probabilities for
  the McNemar test applied to paired binary data.
- [`marginal.probs.2x2()`](https://metinbulus.github.io/pwrss/reference/marginal.probs.2x2.md)
  : Conversion from marginal probabilities to joint probabilities for
  the McNemar test applied to paired binary data.
- [`means.to.d()`](https://metinbulus.github.io/pwrss/reference/means.to.d.md)
  : Conversion from Means and Standard Deviations to Cohen's d
- [`means.to.etasq()`](https://metinbulus.github.io/pwrss/reference/means.to.etasq.md)
  : Conversion from Means and Standard Deviations to Cohen's f and
  Eta-squared
- [`probs.to.h()`](https://metinbulus.github.io/pwrss/reference/probs.to.h.md)
  : Conversion from Probability Difference to Cohen's h
- [`probs.to.w()`](https://metinbulus.github.io/pwrss/reference/probs.to.w.md)
  : Conversion from Probabilities to Cohen's w
- [`q.to.cors()`](https://metinbulus.github.io/pwrss/reference/q.to.cors.md)
  : Conversion from a Cohen's q to a correlation difference
- [`rsq.to.f()`](https://metinbulus.github.io/pwrss/reference/rsq.to.f.md)
  : Conversion from R-squared to Cohen's f
- [`z.to.cor()`](https://metinbulus.github.io/pwrss/reference/z.to.cor.md)
  : Conversion from a z-value to a correlation (inverse Fisher's
  z-transformation)

## Lambda Prime

Distribution functions for the lambda prime distribution

- [`dlambdap()`](https://metinbulus.github.io/pwrss/reference/lambdap.md)
  [`plambdap()`](https://metinbulus.github.io/pwrss/reference/lambdap.md)
  [`qlambdap()`](https://metinbulus.github.io/pwrss/reference/lambdap.md)
  [`rlambdap()`](https://metinbulus.github.io/pwrss/reference/lambdap.md)
  : Distribution functions for the Lambda prime / non-central Lambda
  distribution
