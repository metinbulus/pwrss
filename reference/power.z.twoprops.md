# Power Analysis for Testing Difference Between Two Proportions (Normal Approximation Method)

Calculates power or sample size (only one can be NULL at a time) for two
proportions using the normal approximation method.

Validated via G\*Power and PASS documentation.

## Usage

``` r
power.z.twoprops(
  prob1,
  prob2,
  margin = 0,
  n.ratio = 1,
  n2 = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  arcsine = FALSE,
  correct = FALSE,
  paired = FALSE,
  rho.paired = 0.5,
  std.error = c("pooled", "unpooled"),
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- prob1:

  probability of success in the first group.

- prob2:

  probability of success in the second group.

- margin:

  ignorable `prob1` - `prob2` difference. For two one-sided tests
  provide lower and upper margins in the form of `c(lower, upper)`.

- n.ratio:

  sample size ratio (n1 / n2).

- n2:

  integer; sample size for the second group.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided".

- arcsine:

  logical; whether arcsine transformation should be applied. Note that
  this only applies to independent proportions without continuity
  correction.

- correct:

  logical; whether Yates' continuity correction should be applied to the
  test statistic. Ignored for the paired test.

- paired:

  logical; if `TRUE` samples are paired. `FALSE` by default.

- rho.paired:

  correlation between paired observations.

- std.error:

  character; whether to calculate standard error using "pooled" or
  "unpooled" standard deviation. Ignored for the paired test.

- ceiling:

  logical; `TRUE` rounds up sample size in each group.

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

  type of the test, which is "z" or "exact".

- power:

  statistical power \\(1-\beta)\\.

- mean:

  mean of the alternative distribution.

- sd:

  standard deviation of the alternative distribution.

- null.mean:

  mean of the null distribution.

- null.sd:

  standard deviation of the null distribution.

- z.alpha:

  critical value(s).

- n:

  sample size in the form of c(n1, n2) (applies to independent
  proportions).

- n.total:

  total sample size (applies to independent proportions).

- n.paired:

  paired sample size (applies to paired proportions).

## Details

- NB: The `pwrss.z.2props()` function is deprecated, but it will remain
  available as a wrapper for the `power.z.twoprops()` function during a
  transition period.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

## Examples

``` r
  # power
  power.z.twoprops(prob1 = 0.65, prob2 = 0.60,
                   alpha = 0.05, n2 = 500,
                   alternative = "one.sided")
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Independent Proportions
#> 
#>   Method : Normal Approximation
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob1 - prob2 <= 0
#>   H1 (Alternative) : prob1 - prob2  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 500 and 500
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.505
#>   Statistical Power    = 0.495  <<
#> 

  # sample size
  power.z.twoprops(prob1 = 0.65, prob2 = 0.60,
                   alpha = 0.05, power = 0.80,
                   alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Independent Proportions
#> 
#>   Method : Normal Approximation
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob1 - prob2 <= 0
#>   H1 (Alternative) : prob1 - prob2  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 1159 and 1159  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
