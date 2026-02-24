# Power Analysis for Fisher's Exact Test (Independent Proportions)

Calculates power or sample size for Fisher's exact test on independent
binary outcomes. Approximate and exact methods are available.

Validated using the PASS documentation and G\*Power.

## Usage

``` r
power.exact.fisher(
  prob1,
  prob2,
  n.ratio = 1,
  n2 = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
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

  n1 / n2 ratio.

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

- method:

  character; method used for power calculation. "exact" specifies
  Fisher's exact test, while "approximate" refers to the Z-Test based on
  the normal approximation.

- ceiling:

  logical; if `TRUE` rounds up sample size in each group.

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

  type of the test, which is "exact" or "z".

- odds.ratio:

  odds ratio.

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

  sample sizes for the first and second groups, specified as c(n1, n2).

- n.total:

  total sample size, which is sum of cell frequencies in the 2 x 2 table
  (f11 + f10 + f01 + f00), or number of rows in a data frame with group
  variable stacked.

## References

Bennett, B. M., & Hsu, P. (1960). On the power function of the exact
test for the 2 x 2 contingency table. *Biometrika, 47*(3/4), 393-398.
https://doi.org/10.2307/2333309

Fisher, R. A. (1935). The logic of inductive inference. *Journal of the
Royal Statistical Society, 98*(1), 39-82.
https://doi.org/10.2307/2342435

## Examples

``` r
# example data for a randomized controlled trial
# subject  group    success
# <int>    <dbl>      <dbl>
#   1        1          1
#   2        0          1
#   3        1          0
#   4        0          1
#   5        1          1
#   ...     ...        ...
#   100      0          0

# prob1 = mean(success | group = 1)
# prob2 = mean(success | group = 0)

# post-hoc exact power
power.exact.fisher(prob1 = 0.60, prob2 = 0.40, n2 = 50)
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
#>   H0 (Null)        : prob1 - prob2  = 0
#>   H1 (Alternative) : prob1 - prob2 != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 50 and 50
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.538
#>   Statistical Power    = 0.462  <<
#> 

# we may have 2 x 2 joint probs such as
# -------------------------------------
#             | group (1) | group (0) |
# -------------------------------------
# success (1) |  0.24    |   0.36     |
# -------------------------------------
# success (0) |  0.16    |   0.24     |
# -------------------------------------

# convert joint probs to marginal probs
marginal.probs.2x2(prob11 = 0.24, prob10 = 0.36,
                   prob01 = 0.16, prob00 = 0.24)
#> prob1 prob2   rho 
#>   0.6   0.4   0.0 
```
