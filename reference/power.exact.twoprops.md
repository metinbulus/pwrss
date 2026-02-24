# Power Analysis for Testing Difference Between Two Proportions (Exact Method)

Calculates power or sample size (only one can be NULL at a time) for two
proportions using the exact method. The function is a wrapper for
`power.exact.mcnemar` (if `paired` == TRUE), or `power.exact.fisher` (if
`paired` == FALSE)

Validated via G\*Power and PASS documentation.

## Usage

``` r
power.exact.twoprops(
  prob1,
  prob2,
  n.ratio = 1,
  n2 = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
  paired = FALSE,
  rho.paired = 0.5,
  method = c("exact", "approximate"),
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

  character; the direction or type of the hypothesis test: "two.sided"
  or "one.sided".

- paired:

  logical; if `TRUE` samples are paired. `FALSE` by default.

- rho.paired:

  correlation between paired observations.

- method:

  character; whether to use "approximate" or "exact" method. Default is
  `"exact"` (only in the `power.exact.twoprops()` function).

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

- delta:

  difference between `prob1` and `prob2`

- odds.ratio:

  Odds-ratio \\(prob1 / (1 - prob1)) / (prob2 / (1 - prob2))\\

- size:

  ... (applies to paired proportions).

- prob:

  ... (applies to paired proportions).

- null.prob:

  ... (applies to paired proportions).

- binom.alpha:

  critical value(s) (applies to paired proportions).

- mean:

  mean of the alternative distribution.

- sd:

  standard deviation of the alternative distribution.

- null.mean:

  mean of the null distribution.

- null.sd:

  standard deviation of the null distribution.

- alternative:

  standard deviation of the null distribution.

- z.alpha:

  critical value(s).

- power:

  statistical power \\(1 - \beta)\\.

- n:

  sample size in the form of c(n1, n2) (applies to independent
  proportions).

- n.total:

  total sample size (applies to independent proportions).

- n.paired:

  paired sample size (applies to paired proportions).

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

## Examples

``` r
  # power
  power.exact.twoprops(prob1 = 0.70, prob2 = 0.60,
                       alpha = 0.05, n2 = 500,
                       alternative = "one.sided")
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Independent Proportions
#> 
#>   Method : Fisher's Exact
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
#>   Type 2 Error (beta)  = 0.053
#>   Statistical Power    = 0.947  <<
#> 

  power.exact.twoprops(prob1 = 0.70, prob2 = 0.60,
                       alpha = 0.05, n2 = 500,
                       alternative = "one.sided",
                       paired = TRUE)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Paired Proportions
#> 
#>   Method : McNemar's Exact
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob10 - prob01 <= 0
#>   H1 (Alternative) : prob10 - prob01  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Paired Sample Size   = 500
#>   Type 1 Error (alpha) = 0.040
#>   Type 2 Error (beta)  = 0.001
#>   Statistical Power    = 0.999  <<
#> 

  # sample size
  power.exact.twoprops(prob1 = 0.70, prob2 = 0.60,
                       alpha = 0.05, power = 0.80,
                       alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Independent Proportions
#> 
#>   Method : Fisher's Exact
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
#>   Sample Size          = 302 and 302  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

  power.exact.twoprops(prob1 = 0.70, prob2 = 0.60,
                       alpha = 0.05, power = 0.80,
                       alternative = "one.sided",
                       paired = TRUE)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Paired Proportions
#> 
#>   Method : McNemar's Exact
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob10 - prob01 <= 0
#>   H1 (Alternative) : prob10 - prob01  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Paired Sample Size   = 158  <<
#>   Type 1 Error (alpha) = 0.036
#>   Type 2 Error (beta)  = 0.195
#>   Statistical Power    = 0.805
#> 
```
