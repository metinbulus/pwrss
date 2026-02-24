# Helper function to converts joint probabilities to marginal probabilities for the McNemar test applied to paired binary data.

Helper function to converts joint probabilities to marginal
probabilities for the McNemar test applied to paired binary data.

## Usage

``` r
joint.probs.2x2(prob1, prob2, rho = 0.5, verbose = 1)
```

## Arguments

- prob1:

  (marginal) probability of success in case group (or after).

- prob2:

  (marginal) probability of success in matched-control group (or
  before).

- rho:

  the correlation between case and matched-control, or after and before
  (phi coefficient).

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

- parms:

  list of parameters used in calculation.

- prob11:

  (joint) probability of success in both groups. 'prob11' and 'prob00'
  are known as concordant probs.

- prob10:

  (joint) probability of success in case (or after) but failure in
  matched control (or before). 'prob10' and 'prob01' are known as
  discordant probs.

- prob01:

  (joint) probability of failure in case (or after) but success in
  matched control (or before). prob10' and 'prob01' are known as
  discordant probs.

- prob00:

  (joint) probability of failure in both groups. 'prob11' and 'prob00'
  are known as concordant probs.

## References

Zhang, S., Cao, J., and Ahn, C. (2017). Inference and sample size
calculation for clinical trials with incomplete observations of paired
binary outcomes. *Statistics in Medicine, 36*(4), 581-591.
https://doi.org/10.1002/sim.7168

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

# example summary stats
# prob1 = mean(case) which is 0.55
# prob2 = mean(control) which is 0.45
# rho = cor(case, control) which is 0.4141414


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

# example summary stats
# prob1 = mean(after) which is 0.55
# prob2 = mean(before) which is 0.45
# rho = cor(after, before) which is 0.4141414

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


# we may not have 2 x 2 joint probs
# convert marginal probs to joint probs using summary stats
jp <- joint.probs.2x2(prob1 = 0.55, # mean of case (or after)
                          prob2 = 0.45, # mean of matched control (or before)
                          # correlation b/w matched case-control / before-after
                          rho = 0.4141414)
#>    rho.min    rho.max     prob11     prob10     prob01     prob00 
#> -1.0000000  0.8181818  0.3500000  0.2000000  0.1000000  0.3500000 

# required sample size for exact test
# assuming prob01 and prob10 are population parameters
power.exact.mcnemar(prob01 = jp$prob01,
                    prob10 = jp$prob10,
                    power = 0.80, alpha = 0.05,
                    method = "exact")
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

# convert joint probs to marginal probs and calc phi coefficient (rho)
# these values can be used in other procedures
marginal.probs.2x2(prob11 = 0.35, # mean of case (or after)
                    prob10 = 0.20, # mean of matched control (or before)
                    prob01 = 0.10,
                    prob00 = 0.35)
#>     prob1     prob2       rho 
#> 0.5500000 0.4500000 0.4141414 
```
