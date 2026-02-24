# Power Analysis for Welch's t-Test

Calculates power or sample size (only one can be NULL at a time) for
Welch's t-Tests. Welch's T-Test implementation relies on formulas
proposed by Bulus (2024).

In contrast to previous versions, users can now specify whether their
claims will be based on raw score mean difference with p-values or
standardized mean difference with confidence intervals. While results
typically differ by only a few units, these distinctions can be
particularly consequential in studies with small sample sizes or
high-risk interventions.

Formulas are validated using Monte Carlo simulations (see Bulus, 2024),
G\*Power, <http://powerandsamplesize.com/>, and tables in the PASS
documentation. One key difference between PASS and `pwrss` lies in how
they handle non-inferiority and superiority tests-that is, one-sided
tests defined by a negligible effect margin (implemented as of this
version). PASS shifts the test statistic so that the null hypothesis
assumes a zero effect, treating the negligible margin as part of the
alternative hypothesis. As a result, the test statistic is evaluated
against a central distribution. In contrast, `pwrss` treats the
negligible effect as the true null value, and the test statistic is
evaluated under a non-central distribution. This leads to slight
differences up to third decimal place. To get the same results, reflect
the margin in `null.d` and specify `margin = 0`.

Equivalence tests are implemented in line with Bulus and Polat (2023),
Chow et al. (2018) and Lakens (2017).

## Usage

``` r
power.t.welch(
  d,
  null.d = 0,
  margin = 0,
  var.ratio = 1,
  n.ratio = 1,
  n2 = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  claim.basis = c("md.pval", "smd.ci"),
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- d:

  Cohen's d or Hedges' g.

- null.d:

  Cohen's d or Hedges' g under null, typically 0(zero).

- margin:

  margin - ignorable `d` - `null.d` difference.

- var.ratio:

  variance ratio in the form of sd1 ^ 2 / sd2 ^ 2.

- n.ratio:

  `n1 / n2` ratio (applies to independent samples only)

- n2:

  integer; sample size in the second group (or for the single group in
  paired samples or one-sample).

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". For non-inferiority or superiority
  tests, add or subtract the margin from the null hypothesis value and
  use `alternative = "one.sided"`.

- claim.basis:

  character; "md.pval" when claims are based on raw mean differences and
  p-values, "smd.ci" when claims are based on standardized mean
  differences and confidence intervals.

- ceiling:

  logical; whether sample size should be rounded up. `TRUE` by default.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

- utf:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- parms:

  list of parameters used in calculation.

- test:

  type of the statistical test (T-Test).

- df:

  degrees of freedom.

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- t.alpha:

  critical value(s).

- power:

  statistical power \\(1-\beta)\\.

- n:

  sample size (`n` or `c(n1, n2)`.

## Details

- Use
  [`means.to.d()`](https://metinbulus.github.io/pwrss/reference/means.to.d.md)
  to convert raw means and standard deviations to Cohen's d, and
  [`d.to.cles()`](https://metinbulus.github.io/pwrss/reference/d.to.cles.md)
  to convert Cohen's d to the probability of superiority. Note that this
  interpretation is appropriate only when the underlying distribution is
  approximately normal and the two groups have similar population
  variances.

- NB: The functions
  [`pwrss.z.mean()`](https://metinbulus.github.io/pwrss/reference/power.t.student.md)
  and
  [`pwrss.z.2means()`](https://metinbulus.github.io/pwrss/reference/power.t.student.md)
  are no longer supported. The
  [`pwrss.t.mean()`](https://metinbulus.github.io/pwrss/reference/power.t.student.md)
  and
  [`pwrss.t.2means()`](https://metinbulus.github.io/pwrss/reference/power.t.student.md)
  functions are deprecated, but they will remain available as wrappers
  for
  [`power.t.student()`](https://metinbulus.github.io/pwrss/reference/power.t.student.md)
  or `power.t.welch()` during a transition period.

## References

Bulus, M. (2024). *Robust standard errors and confidence intervals for
standardized mean differences*. https://doi.org/10.31219/osf.io/k6mbs

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2018). *Sample size
calculations in clinical research* (3rd ed.). Taylor & Francis/CRC.

Cohen, J. (1988). *Statistical power analysis for the behavioral
sciences* (2nd ed.). Lawrence Erlbaum Associates.

Lakens, D. (2017). Equivalence tests: A practical primer for t tests,
correlations, and meta-analyses. *Social psychological and personality
science, 8*(4), 355-362. https://doi.org/10.1177/1948550617697177

## Examples

``` r
# see `?pwrss::power.t.student` for examples
```
