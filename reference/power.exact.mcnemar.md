# Power Analysis for McNemar's Exact Test (Paired Proportions)

Calculates power or sample size for McNemar's test on paired binary
outcomes. Approximate and exact methods are available (check references
for details).

Validated using the PASS documentation and G\*Power.

## Usage

``` r
power.exact.mcnemar(
  prob10,
  prob01,
  n.paired = NULL,
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

- prob10:

  (joint) probability of success in case (or after) but failure in
  matched control (or before). 'prob10' and 'prob01' are known as
  discordant probs.

- prob01:

  (joint) probability of failure in case (or after) but success in
  matched control (or before). prob10' and 'prob01' are known as
  discordant probs.

- n.paired:

  number of pairs, which is sum of cell frequencies in the 2 x 2 table
  (f11 + f10 + f01 + f00), or number of rows in a data frame with
  matched variables 'case' and 'control' or 'after' and 'before'.

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

  character; the method used for power calculation. "exact" specifies
  Fisher's exact test, while "approximate" refers to the z-test based on
  the normal approximation.

- ceiling:

  logical; if `TRUE` rounds up sample size in each cell. This procedure
  assumes symmetry for concordant probs, which are 'p11' and 'p00').
  Thus results may differ from other software by a few units. To match
  results set 'ceiling = FALSE'.

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

- n.paired:

  paired sample size, which is sum of cell frequencies in the 2 x 2
  table (f11 + f10 + f01 + f00), or number of rows in a data frame with
  variables 'case' and 'control' or 'after' and 'before'.

## References

Bennett, B. M., & Underwood, R. E. (1970). 283. Note: On McNemar's Test
for the 2 \* 2 Table and Its Power Function. \*Biometrics, 26(2),
339-343. https://doi.org/10.2307/2529083

Connor, R. J. (1987). Sample size for testing differences in proportions
for the paired-sample design. *Biometrics, 43*(1), 207-211.
https://doi.org/10.2307/2531961

Duffy, S. W. (1984). Asymptotic and exact power for the McNemar test and
its analogue with R controls per case. *Biometrics, 40*(4) 1005-1015.
https://doi.org/10.2307/2531151

McNemar, Q. (1947). Note on the sampling error of the difference between
correlated proportions or percentages. *Psychometrika, 12*(2), 153-157.
https://doi.org/10.1007/BF02295996

Miettinen, O. S. (1968). The matched pairs design in the case of
all-or-none responses. *Biometrics, 24*(2), 339-352.
https://doi.org/10.2307/2528039

## Examples

``` r
# example data for a matched case-control design
# subject  case     control
# <int>    <dbl>    <dbl>
#   1        1        1
#   2        0        1
#   3        1        0
#   4        0        1
#   5        1        1
#   ...     ...      ...
#   100      0        0

# example data for a before-after design
# subject  before   after
# <int>    <dbl>    <dbl>
#   1        1        1
#   2        0        1
#   3        1        0
#   4        0        1
#   5        1        1
#   ...     ...      ...
#   100      0        0

# convert to a 2 x 2 frequency table
freqs <- matrix(c(30, 10, 20, 40), nrow = 2, ncol = 2)
colnames(freqs) <- c("control_1", "control_0")
rownames(freqs) <- c("case_1", "case_0")
freqs
#>        control_1 control_0
#> case_1        30        20
#> case_0        10        40

# convert to a 2 x 2 proportion table
props <- freqs / sum(freqs)
props
#>        control_1 control_0
#> case_1       0.3       0.2
#> case_0       0.1       0.4

# discordant pairs (0 and 1, or 1 and 0) in 'props' matrix
# are the sample estimates of prob01 and prob10

# post-hoc exact power
power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10,
                    n.paired = 100, alpha = 0.05,
                    alternative = "two.sided", method = "exact")
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
#>   H0 (Null)        : prob10 - prob01  = 0
#>   H1 (Alternative) : prob10 - prob01 != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Paired Sample Size   = 100
#>   Type 1 Error (alpha) = 0.043
#>   Type 2 Error (beta)  = 0.627
#>   Statistical Power    = 0.373  <<
#> 

# required sample size for exact test
# assuming prob01 and prob10 are population parameters
power.exact.mcnemar(prob10 = 0.20, prob01 = 0.10,
                    power = 0.80, alpha = 0.05,
                    alternative = "two.sided", method = "exact")
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
#>   H0 (Null)        : prob10 - prob01  = 0
#>   H1 (Alternative) : prob10 - prob01 != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Paired Sample Size   = 249  <<
#>   Type 1 Error (alpha) = 0.037
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

# we may not have 2 x 2 joint probs
# convert marginal probs to joint probs
joint.probs.2x2(prob1 = 0.55, # mean of case group (or after)
                    prob2 = 0.45, # mean of matched control group (or before)
                    # correlation between matched case-control or before-after
                    rho = 0.4141414
)
#>    rho.min    rho.max     prob11     prob10     prob01     prob00 
#> -1.0000000  0.8181818  0.3500000  0.2000000  0.1000000  0.3500000 
```
