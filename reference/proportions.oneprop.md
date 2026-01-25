# Power Analysis for the Test of One Proportion (Normal Approximation and Exact Methods)

Calculates power or sample size (only one can be NULL at a time) for
test of a proportion against a constant using normal approximation or
exact method.

Formulas are validated using PASS documentation.

NOTE: The `pwrss.z.prop()` function is deprecated, but it will remain
available as a wrapper for the `power.z.oneprop()` function during the
transition period.

## Usage

``` r
power.z.oneprop(prob, null.prob = 0.50,
                n = NULL, power = NULL, alpha = 0.05,
                alternative = c("two.sided", "one.sided", "two.one.sided"),
                std.error = c("null", "alternative"),
                arcsine = FALSE, correct = FALSE,
                ceiling = TRUE, verbose = TRUE, pretty = FALSE)

power.exact.oneprop(prob, null.prob = 0.50,
                    n = NULL, power = NULL, alpha = 0.05,
                    alternative = c("two.sided", "one.sided", "two.one.sided"),
                    verbose = TRUE, pretty = FALSE)
```

## Arguments

- prob:

  probability of success under alternative.

- null.prob:

  probability of success under null.

- n:

  integer; sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- std.error:

  character; whether to calculate standard error using "null" or
  "alternative" value. "null" by default.

- arcsine:

  logical; whether arcsine transformation should be applied. `FALSE` by
  default. Note that when `arcsine = TRUE`, any specification to
  `correct` and `std.error` will be ignored.

- correct:

  logical; whether Yate's continuity correction should be applied.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". For non-inferiority or superiority
  tests, add margin to the null hypothesis value and use
  `alternative = "one.sided"`.

- ceiling:

  logical; whether sample size should be rounded up. `TRUE` by default.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

- pretty:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- parms:

  list of parameters used in calculation.

- test:

  type of the statistical test ("exact").

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

- power:

  statistical power \\(1-\beta)\\.

- n:

  sample size.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24(3), 2207-2328.
[doi:10.29299/kefad.1209913](https://doi.org/10.29299/kefad.1209913)

## Examples

``` r
# power
power.z.oneprop(prob = 0.45, null.prob = 0.50,
                alpha = 0.05, n = 500,
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> One Proportion
#> 
#>   Method                 : Normal Approximation
#>   Continuity Correction  : FALSE
#>   Arcsine Transformation : FALSE
#>   Standard Error         : Calculated From Null
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)        : prob - null.prob >= 0
#>   H1 (Alt. Claim)        : prob - null.prob < 0
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size           = 500
#>   Type 1 Error (alpha)  = 0.050
#>   Type 2 Error (beta)   = 0.276
#>   Statistical Power     = 0.724  <<
#> 

power.exact.oneprop(prob = 0.45, null.prob = 0.50,
                    alpha = 0.05, n = 500,
                    alternative = "one.sided")
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> One Proportion
#> 
#>   Method                 : Exact
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)        : prob - null.prob >= 0
#>   H1 (Alt. Claim)        : prob - null.prob < 0
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size           = 500
#>   Type 1 Error (alpha)  = 0.050
#>   Type 2 Error (beta)   = 0.279
#>   Statistical Power     = 0.721  <<
#> 



# sample size
power.z.oneprop(prob = 0.45, null.prob = 0.50,
                alpha = 0.05, power = 0.80,
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One Proportion
#> 
#>   Method                 : Normal Approximation
#>   Continuity Correction  : FALSE
#>   Arcsine Transformation : FALSE
#>   Standard Error         : Calculated From Null
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)        : prob - null.prob >= 0
#>   H1 (Alt. Claim)        : prob - null.prob < 0
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size           = 617  <<
#>   Type 1 Error (alpha)  = 0.050
#>   Type 2 Error (beta)   = 0.200
#>   Statistical Power     = 0.8
#> 

power.exact.oneprop(prob = 0.45, null.prob = 0.50,
                    alpha = 0.05, power = 0.80,
                    alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One Proportion
#> 
#>   Method                 : Exact
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)        : prob - null.prob >= 0
#>   H1 (Alt. Claim)        : prob - null.prob < 0
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size           = 633  <<
#>   Type 1 Error (alpha)  = 0.050
#>   Type 2 Error (beta)   = 0.197
#>   Statistical Power     = 0.803
#> 
```
