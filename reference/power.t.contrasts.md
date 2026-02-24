# Power Analysis for One-, Two-, Three-Way ANCOVA Contrasts and Multiple Comparisons (T-Tests)

Calculates power or sample size for one-, two-, three-Way ANCOVA
contrasts and multiple comparisons. The `power.t.contrasts()` function
permits to test multiple contrasts (multiple comparisons) and also
allows adjustment to alpha due to multiple testing. Furthermore,
`power.t.contrasts()` accepts an object returned from the
[`power.f.ancova.shieh()`](https://metinbulus.github.io/pwrss/reference/power.f.ancova.shieh.md)
function for convenience. Beware that, in this case, all other arguments
are ignored except `alpha` and `adjust.alpha`.

Formulas are validated using examples and tables in Shieh (2017).

## Usage

``` r
power.t.contrasts(
  x = NULL,
  mu.vector = NULL,
  sd.vector = NULL,
  n.vector = NULL,
  p.vector = NULL,
  r.squared = 0,
  k.covariates = 1,
  contrast.matrix = NULL,
  power = NULL,
  alpha = 0.05,
  adjust.alpha = c("none", "tukey", "bonferroni", "holm", "hochberg", "hommel", "BH",
    "BY", "fdr"),
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- x:

  object; an object returned from the
  [`power.f.ancova.shieh()`](https://metinbulus.github.io/pwrss/reference/power.f.ancova.shieh.md)
  function.

- mu.vector:

  vector; adjusted means (or estimated marginal means) for each level of
  a factor. Ignored when 'x' is specified.

- sd.vector:

  vector; unadjusted standard deviations for each level of a factor.
  Ignored when 'x' is specified.

- n.vector:

  vector; sample sizes for each level of a factor. Ignored when 'x' is
  specified.

- p.vector:

  vector; proportion of total sample size in each level of a factor.
  These proportions should sum to one. Ignored when 'x' is specified.

- r.squared:

  explanatory power of covariates (R-squared) in the ANCOVA model.
  Ignored when 'x' is specified.

- k.covariates:

  Number of covariates in the ANCOVA model. Ignored when 'x' is
  specified.

- contrast.matrix:

  vector or matrix; contrasts should not be confused with the model
  (design) matrix. Rows of contrast matrix indicate independent vector
  of contrasts summing to zero. The default contrast matrix is
  constructed using deviation coding scheme (a.k.a. effect coding).
  Columns in the contrast matrix indicate number of levels or groups (or
  cells in factorial designs). Ignored when 'x' is specified.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\. Ignored when 'x' is
  specified.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\. Note that this should
  be specified even if 'x' is specified. The 'alpha' value within the
  'x' object pertains to the omnibus test, NOT the test of contrasts.

- adjust.alpha:

  character; one of the methods in c("none", "tukey", "bonferroni",
  "holm", "hochberg", "hommel", "BH", "BY", "fdr") to control Type 1
  error. See
  [`?stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- ceiling:

  logical; `TRUE` by default. If `FALSE` sample sizes in each cell are
  NOT rounded up.

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

- contrast:

  contrast number (one contrast per line).

- comparison:

  which factor levels are compared (one contrast per line).

- psi:

  contrast-weighted mean difference (one contrast per line).

- d:

  contrast-weighted standardized mean difference (one contrast per
  line).

- ncp:

  non-centrality parameter for the alternative (one contrast per line).

- df:

  degrees of freedom (one contrast per line).

- t.alpha:

  critical values (one contrast per line).

- n.total:

  total sample size (one contrast per line).

- power:

  statistical power \\(1-\beta)\\ (one contrast per line).

## Details

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `mu` or `mu.vec` instead of
`mu.vector`, or such as `k` or `k.cov` instead of `k.covariates`.

## References

Shieh, G. (2017). Power and sample size calculations for contrast
analysis in ANCOVA. *Multivariate Behavioral Research, 52*(1), 1-11.
https://doi.org/10.1080/00273171.2016.1219841

## Examples

``` r
# see `?pwrss::power.f.ancova.shieh` for further examples

# dummy coding example
contrast.object <- factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = 0)
contrast.matrix <- contrast.object[["contrast.matrix"]]
power.t.contrasts(mu.vector = c(0.15, 0.30, 0.20),
                  sd.vector = c(1,    1,    1),
                  p.vector  = c(1/3,  1/3,  1/3),
                  r.squared = 0.50, k.covariates = 1,
                  contrast.matrix = contrast.matrix,
                  power = 0.80,
                  alpha = 0.05, adjust.alpha = "holm")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Multiple Contrast Analyses (T-Tests)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : psi  = 0
#>   H1 (Alternative) : psi != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>  contr comparison   psi      d    ncp n.total power
#>      1  A1 <=> A3 -0.05 -0.071 -3.084   11412   0.8
#>      2  A2 <=> A3  0.10  0.141  3.085    2856   0.8
#> 
```
