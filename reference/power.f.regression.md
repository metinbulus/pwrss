# Power Analysis for Linear Regression: R-squared or R-squared Change (F-Test)

Calculates power or sample size (only one can be NULL at a time) to test
R-squared deviation from 0 (zero) in linear regression or to test
R-squared change between two linear regression models. The test of
R-squared change is often used to evaluate incremental contribution of a
set of predictors in hierarchical linear regression.

Formulas are validated using Monte Carlo simulation, G\*Power, and
tables in the PASS documentation.

## Usage

``` r
power.f.regression(
  r.squared.change = NULL,
  margin = 0,
  k.total,
  k.tested = k.total,
  n = NULL,
  power = NULL,
  alpha = 0.05,
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- r.squared.change:

  R-squared (or R-squared change).

- margin:

  margin - ignorable R-squared (or R-squared change).

- k.total:

  integer; total number of predictors.

- k.tested:

  integer; number of predictors in the subset of interest. By default
  `k.tested = k.total`, which implies that one is interested in the
  contribution of all predictors, and tests whether R-squared value is
  different from 0 (zero).

- n:

  integer; sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

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

  type of the statistical test (F-Test).

- df1:

  numerator degrees of freedom.

- df2:

  denominator degrees of freedom.

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- f.alpha:

  critical value.

- power:

  statistical power \\(1-\beta)\\.

- n:

  sample size.

## Details

- NB: The `pwrss.f.regression` function and its alias `pwrss.f.reg()`
  are deprecated, but they will remain available as a wrapper for the
  `power.f.regression()` function during a transition period.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
# in the outcome (R-squared = 0.15).
power.f.regression(r.squared = 0.15,
                   k.total = 3, # total number of predictors
                   power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : R-squared = 0
#>   H1 (Alternative) : R-squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 66  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

# adding two more variables will increase R-squared
# from 0.15 (with 3 predictors) to 0.25 (with 3 + 2 predictors)
power.f.regression(r.squared.change = 0.10, # R-squared change
                   k.total = 5, # total number of predictors
                   k.tested = 2, # predictors to be tested
                   power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Hierarchical Linear Regression (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Change in R-squared = 0
#>   H1 (Alternative) : Change in R-squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 90  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 
```
