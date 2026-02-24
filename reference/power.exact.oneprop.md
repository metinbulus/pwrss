# Power Analysis for the Test of One Proportion (Exact Method)

Calculates power or sample size (only one can be NULL at a time) for
test of a proportion against a constant using the exact method.

Formulas are validated using PASS documentation.

## Usage

``` r
power.exact.oneprop(
  prob,
  null.prob = 0.5,
  n = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  verbose = 1,
  utf = FALSE
)
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

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". For non-inferiority or superiority
  tests, add margin to the null hypothesis value and use
  `alternative = "one.sided"`.

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

  type of the statistical test ("exact").

- delta:

  difference between `prob` and `null.prob`

- odds.ratio:

  Odds-ratio \\(prob / (1 - prob)) / (null.prob / (1 - null.prob))\\

- prob:

  probability of success under alternative.

- null.prob:

  probability of success under null.

- binom.alpha:

  critical value(s).

- power:

  statistical power \\(1 - \beta)\\.

- n:

  sample size.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

## Examples

``` r
# power'
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
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob - null.prob >= 0
#>   H1 (Alternative) : prob - null.prob  < 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 500
#>   Type 1 Error (alpha) = 0.049
#>   Type 2 Error (beta)  = 0.279
#>   Statistical Power    = 0.721  <<
#> 

# sample size
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
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob - null.prob >= 0
#>   H1 (Alternative) : prob - null.prob  < 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 633  <<
#>   Type 1 Error (alpha) = 0.047
#>   Type 2 Error (beta)  = 0.197
#>   Statistical Power    = 0.803
#> 
```
