# Power Analysis for One-, Two-, Three-Way ANCOVA Contrasts and Multiple Comparisons (T-Tests)

Calculates power or sample size for a single one-, two-, three-Way
ANCOVA contrast.

Formulas are validated using examples and tables in Shieh (2017).

## Usage

``` r
power.t.contrast(
  mu.vector,
  sd.vector,
  n.vector = NULL,
  p.vector = NULL,
  contrast.vector,
  r.squared = 0,
  k.covariates = 1,
  power = NULL,
  alpha = 0.05,
  tukey.kramer = FALSE,
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- mu.vector:

  vector; adjusted means (or estimated marginal means) for each level of
  a factor.

- sd.vector:

  vector; unadjusted standard deviations for each level of a factor.

- n.vector:

  vector; sample sizes for each level of a factor.

- p.vector:

  vector; proportion of total sample size in each level of a factor.
  These proportions should sum to one.

- contrast.vector:

  vector; a single contrast in the form of a vector with as many
  elements as number of levels or groups (or cells in factorial
  designs). Ignored when 'x' is specified.

- r.squared:

  explanatory power of covariates (R-squared) in the ANCOVA model.

- k.covariates:

  Number of covariates in the ANCOVA model.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- tukey.kramer:

  logical; `FALSE` by default. If `TRUE` adjustments will be made to
  control Type 1 error.

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

- psi:

  contrast-weighted mean difference.

- d:

  contrast-weighted standardized mean difference.

- df:

  degrees of freedom.

- t.alpha:

  critical values.

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- power:

  statistical power \\(1-\beta)\\.

- n.vector:

  sample sizes for each level of a factor.

- n.total:

  total sample size.

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
# dummy coding example (uses the first contrast from a three-level- / two-contrasts-design)
contrast.object <- factorial.contrasts(factor.levels = 3, coding = "treatment", verbose = 0)
contrast.vector <- contrast.object[["contrast.matrix"]][1, ]
power.t.contrast(mu.vector = c(0.15, 0.30, 0.20),
                 sd.vector = c(1,    1,    1),
                 p.vector  = c(1/3,  1/3,  1/3),
                 r.squared = 0.50, k.covariates = 1,
                 contrast.vector = contrast.vector,
                 power = 0.80, alpha = 0.05)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Single Contrast Analysis (T-Test)
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
#>   Total Sample Size    = 9423  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
