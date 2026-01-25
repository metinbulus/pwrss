# Power Analysis for Testing Difference Between Two Proportions (Normal Approximation and Exact Methods)

Calculates power or sample size (only one can be NULL at a time) for two
proportions using normal approximation method.

Validated via G\*Power and PASS documentation.

NOTE: The `pwrss.z.2props()` function is deprecated, but it will remain
available as a wrapper for the `power.z.twoprops()` function during the
transition period.

## Usage

``` r
power.z.twoprops(prob1, prob2, margin = 0,
                 n2 = NULL, n.ratio = 1,
                 power = NULL, alpha = 0.05,
                 alternative = c("two.sided", "one.sided", "two.one.sided"),
                 arcsine = FALSE, correct = FALSE,
                 paired = FALSE, rho.paired = 0.50,
                 std.error = c("pooled", "unpooled"),
                 ceiling = TRUE, verbose = TRUE, pretty = FALSE)

power.exact.twoprops(prob1, prob2, n2 = NULL, n.ratio = 1,
                     power = NULL, alpha = 0.05,
                     alternative = c("two.sided", "one.sided"),
                     paired = FALSE, rho.paired = 0.50,
                     method = c("exact", "approximate"),
                     ceiling = TRUE, verbose = TRUE, pretty = FALSE)
```

## Arguments

- prob1:

  probability of success in the first group.

- prob2:

  probability of success in the second group.

- margin:

  ignorable `prob1` - `prob2` difference. For two one-sided tests
  provide lower and upper margins in the form of c(lower, upper).

- n2:

  integer; sample size for the second group.

- n.ratio:

  sample size ratio (n1 / n2).

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- paired:

  logical; if `TRUE` samples are paired. `FALSE` by default.

- rho.paired:

  correlation between paired observations.

- method:

  character; whether to use "approximate" or "exact" method. Default is
  "exact" (only in the `power.exact.twoprops()` function).

- arcsine:

  logical; whether arcsine transformation should be applied. Note that
  this only applies to independent proportions without continuity
  correction.

- correct:

  logical; whether Yates' continuity correction should be applied to the
  test statistic. Ignored for the paired test.

- std.error:

  character; whether to calculate standard error using "pooled" or
  "unpooled" standard deviation. Ignored for the paired test.

- alternative:

  character; direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided".

- ceiling:

  logical; `TRUE` rounds up sample size in each group.

- verbose:

  logical; `TRUE` prints the output on the console.

- pretty:

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

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24(3), 2207-2328.
[doi:10.29299/kefad.1209913](https://doi.org/10.29299/kefad.1209913)

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
#>   Method          : Normal Approximation
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : prob1 - prob2 <= 0 
#>   H1 (Alt. Claim) : prob1 - prob2 > 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
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
#>   Method          : Normal Approximation
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : prob1 - prob2 <= 0 
#>   H1 (Alt. Claim) : prob1 - prob2 > 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 1159 and 1159  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.8
#> 
```
