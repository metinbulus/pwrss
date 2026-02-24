# Power Analysis for Student's t-Test

Calculates power or sample size (only one can be NULL at a time) for
Student's t-Test.

In contrast to previous versions, users can now specify whether their
claims will be based on raw score mean difference with P-values or
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
power.t.student(
  d,
  null.d = 0,
  margin = 0,
  n2 = NULL,
  n.ratio = 1,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  design = c("independent", "paired", "one.sample"),
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

- n2:

  integer; sample size in the second group (or for the single group in
  paired samples or one-sample).

- n.ratio:

  `n1 / n2` ratio (applies to independent samples only)

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

- design:

  character; "independent", "paired" or "one.sample".

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

  sample size (`n` or `c(n1, n2)` depending on the design.

## Details

- Use
  [`means.to.d()`](https://metinbulus.github.io/pwrss/reference/means.to.d.md)
  to convert raw means and standard deviations to Cohen's d, and
  [`d.to.cles()`](https://metinbulus.github.io/pwrss/reference/d.to.cles.md)
  to convert Cohen's d to the probability of superiority. Note that this
  interpretation is appropriate only when the underlying distribution is
  approximately normal and the two groups have similar population
  variances.

- NB: The functions `pwrss.z.mean()` and `pwrss.z.2means()` are no
  longer supported. The `pwrss.t.mean()` and `pwrss.t.2means()`
  functions are deprecated, but they will remain available as wrappers
  for `power.t.student()` or
  [`power.t.welch()`](https://metinbulus.github.io/pwrss/reference/power.t.welch.md)
  functions during a transition period.

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
#######################
# Independent Samples #
#######################

## difference between group 1 and group 2 is not equal to zero
## targeting minimal difference of Cohen'd = 0.20
## non-parametric
power.np.wilcoxon(d = 0.20,
                  power = 0.80,
                  alternative = "two.sided",
                  design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d  = 0
#>   H1 (Alternative) : d - null.d != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 412 and 412  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## parametric
power.t.student(d = 0.20,
                power = 0.80,
                alternative = "two.sided",
                design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d  = 0
#>   H1 (Alternative) : d - null.d != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 394 and 394  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## when sample size ratio and group variances differ
power.t.welch(d = 0.20,
              n.ratio = 2,
              var.ratio = 2,
              power = 0.80,
              alternative = "two.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Welch's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d  = 0
#>   H1 (Alternative) : d - null.d != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 474 and 237  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 


## difference between group 1 and group 2 is greater than zero
## targeting minimal difference of Cohen'd = 0.20
## non-parametric
power.np.wilcoxon(d = 0.20,
                  power = 0.80,
                  alternative = "one.sided",
                  design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= 0
#>   H1 (Alternative) : d - null.d  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 325 and 325  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## parametric
power.t.student(d = 0.20,
                power = 0.80,
                alternative = "one.sided",
                design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= 0
#>   H1 (Alternative) : d - null.d  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 310 and 310  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## when sample size ratio and group variances differ
power.t.welch(d = 0.20,
              n.ratio = 2,
              var.ratio = 2,
              power = 0.80,
              alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Welch's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= 0
#>   H1 (Alternative) : d - null.d  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 372 and 186  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 


## mean of group 1 is practically not smaller than mean of group 2
## targeting minimal difference of Cohen'd = 0.20 and can be as small as -0.05
## non-parametric
power.np.wilcoxon(d = 0.20,
                  margin = -0.05,
                  power = 0.80,
                  alternative = "one.sided",
                  design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 208 and 208  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## parametric
power.t.student(d = 0.20,
                margin = -0.05,
                power = 0.80,
                alternative = "one.sided",
                design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 199 and 199  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## when sample size ratio and group variances differ
power.t.welch(d = 0.20,
              margin = -0.05,
              n.ratio = 2,
              var.ratio = 2,
              power = 0.80,
              alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Welch's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 238 and 119  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 


## mean of group 1 is practically greater than mean of group 2
## targeting minimal difference of Cohen'd = 0.20 and can be as small as 0.05
## non-parametric
power.np.wilcoxon(d = 0.20,
                  margin = 0.05,
                  power = 0.80,
                  alternative = "one.sided",
                  design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 578 and 578  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## parametric
power.t.student(d = 0.20,
                margin = 0.05,
                power = 0.80,
                alternative = "one.sided",
                design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 552 and 552  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## when sample size ratio and group variances differ
power.t.welch(d = 0.20,
              margin = 0.05,
              n.ratio = 2,
              var.ratio = 2,
              power = 0.80,
              alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Welch's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 662 and 331  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 


## mean of group 1 is practically same as mean of group 2
## targeting minimal difference of Cohen'd = 0
## and can be as small as -0.05 or as high as 0.05
## non-parametric
power.np.wilcoxon(d = 0,
                  margin = c(-0.05, 0.05),
                  power = 0.80,
                  alternative = "two.one.sided",
                  design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= min(margin) and
#>                      d - null.d <= max(margin)
#>   H1 (Alternative) : d - null.d  < min(margin) or
#>                      d - null.d  > max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 7175 and 7175  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## parametric
power.t.student(d = 0,
                margin = c(-0.05, 0.05),
                power = 0.80,
                alternative = "two.one.sided",
                design = "independent")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= min(margin) or
#>                      d - null.d >= max(margin)
#>   H1 (Alternative) : d - null.d  > min(margin) and
#>                      d - null.d  < max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 6852 and 6852  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## when sample size ratio and group variances differ
power.t.welch(d = 0,
              margin = c(-0.05, 0.05),
              n.ratio = 2,
              var.ratio = 2,
              power = 0.80,
              alternative = "two.one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Welch's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= min(margin) or
#>                      d - null.d >= max(margin)
#>   H1 (Alternative) : d - null.d  > min(margin) and
#>                      d - null.d  < max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 8222 and 4111  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 


##################
# Paired Samples #
##################

## difference between time 1 and time 2 is not equal to zero
## targeting minimal difference of Cohen'd = -0.20
## non-parametric
power.np.wilcoxon(d = -0.20,
                  power = 0.80,
                  alternative = "two.sided",
                  design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d  = 0
#>   H1 (Alternative) : d - null.d != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 208  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## parametric
power.t.student(d = -0.20,
                power = 0.80,
                alternative = "two.sided",
                design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Paired Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d  = 0
#>   H1 (Alternative) : d - null.d != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 199  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.198
#>   Statistical Power    = 0.802
#> 

## difference between time 1 and time 2 is less than zero
## targeting minimal difference of Cohen'd = -0.20
## non-parametric
power.np.wilcoxon(d = -0.20,
                  power = 0.80,
                  alternative = "one.sided",
                  design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= 0
#>   H1 (Alternative) : d - null.d  < 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 164  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.198
#>   Statistical Power    = 0.802
#> 

## parametric
power.t.student(d = -0.20,
                power = 0.80,
                alternative = "one.sided",
                design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Paired Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= 0
#>   H1 (Alternative) : d - null.d  < 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 156  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## mean of time 1 is practically not greater than mean of time 2
## targeting minimal difference of Cohen'd = -0.20 and can be as small as 0.05
## non-parametric
## non-parametric
power.np.wilcoxon(d = 0.20,
                  margin = 0.05,
                  power = 0.80,
                  alternative = "one.sided",
                  design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 291  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## parametric
power.t.student(d = 0.20,
                margin = 0.05,
                power = 0.80,
                alternative = "one.sided",
                design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Paired Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 278  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## mean of time 1 is practically greater than mean of time 2
## targeting minimal difference of Cohen'd = -0.20 and can be as small as -0.05
## non-parametric
power.np.wilcoxon(d = 0.20,
                  margin = -0.05,
                  power = 0.80,
                  alternative = "one.sided",
                  design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 105  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.198
#>   Statistical Power    = 0.802
#> 

## parametric
power.t.student(d = 0.20,
                margin = -0.05,
                power = 0.80,
                alternative = "one.sided",
                design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Paired Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 100  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 


## mean of time 1 is practically same as mean of time 2
## targeting minimal difference of Cohen'd = 0
## and can be as small as -0.05 or as high as 0.05
## non-parametric
## non-parametric
power.np.wilcoxon(d = 0,
                  margin = c(-0.05, 0.05),
                  power = 0.80,
                  alternative = "two.one.sided",
                  design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= min(margin) and
#>                      d - null.d <= max(margin)
#>   H1 (Alternative) : d - null.d  < min(margin) or
#>                      d - null.d  > max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 3589  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## parametric
power.t.student(d = 0,
                margin = c(-0.05, 0.05),
                power = 0.80,
                alternative = "two.one.sided",
                design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Paired Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= min(margin) or
#>                      d - null.d >= max(margin)
#>   H1 (Alternative) : d - null.d  > min(margin) and
#>                      d - null.d  < max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 3427  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
