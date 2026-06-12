# Power Analysis for the Generic Binomial Test

Calculates power or find Probability (Non-Centrality) for the generic
binomial test with (optional) Type 1 and Type 2 error plots.

## Usage

``` r
power.binom.test(
  power = NULL,
  size = NULL,
  prob = NULL,
  null.prob = 0.5,
  req.sign = "+",
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  plot = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- power:

  statistical power \\(1 - \beta)\\; either `power`, `size`, or `prob`
  needs to be NULL (and is then estimated).

- size:

  number of trials (zero or more); either `power`, `size`, or `prob`
  needs to be NULL (and is then estimated).

- prob:

  probability of success on each trial under alternative; either
  `power`, `size` or `prob` needs to be NULL (and is then estimated).

- null.prob:

  probability of success on each trial under null.

- req.sign:

  whether 'prob' is expected to be greater '+1', less than '-1', or
  within '0' the null.prob' bounds.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". For non-inferiority or superiority
  tests, add or subtract the margin from the null hypothesis value and
  use alternative = "one.sided".

- plot:

  logical; `FALSE` switches off Type 1 and Type 2 error plot. `TRUE` by
  default.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

- utf:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- power:

  statistical power \\(1 - \beta)\\.

- size:

  number of trials (zero or more).

- prob:

  probability of success on each trial under alternative.

- null.prob:

  probability of success on each trial under null.

- alpha:

  type 1 error rate.

- alternative:

  direction or type of the hypothesis test.

- binom.alpha:

  critical value(s).

## Examples

``` r
# one-sided
power.binom.test(size = 200, prob = 0.6, null.prob = 0.5, alpha = 0.05,
                 alternative = "one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob <= null.prob
#>   H1 (Alternative) : prob  > null.prob
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Target NCP (prob)    = 0.600 (vs. null.prob = 0.500)
#>   Number of Trials     = 200
#>   Type 1 Error (alpha) = 0.038
#>   Type 2 Error (beta)  = 0.140
#>   Statistical Power    = 0.860  <<
#> 
power.binom.test(power = 0.80, size = 200, req.sign = "+", null.prob = 0.5,
                 alpha = 0.05, alternative = "one.sided")

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE NCP CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob <= null.prob
#>   H1 (Alternative) : prob  > null.prob
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Target NCP (prob)    = 0.592 (vs. null.prob = 0.500)  <<
#>   Number of Trials     = 200
#>   Type 1 Error (alpha) = 0.038
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# two-sided
power.binom.test(size = 200, prob = 0.4, null.prob = 0.5, alpha = 0.05,
                 alternative = "two.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob  = null.prob
#>   H1 (Alternative) : prob != null.prob
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Target NCP (prob)    = 0.400 (vs. null.prob = 0.500)
#>   Number of Trials     = 200
#>   Type 1 Error (alpha) = 0.040
#>   Type 2 Error (beta)  = 0.213
#>   Statistical Power    = 0.787  <<
#> 
power.binom.test(power = 0.80, size = 200, req.sign = "+", null.prob = 0.5,
                 alpha = 0.05, alternative = "two.sided")

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE NCP CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob  = null.prob
#>   H1 (Alternative) : prob != null.prob
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Target NCP (prob)    = 0.602 (vs. null.prob = 0.500)  <<
#>   Number of Trials     = 200
#>   Type 1 Error (alpha) = 0.040
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# equivalence
power.binom.test(size = 200, prob = 0.5, null.prob = c(0.4, 0.6),
                 alpha = 0.05, alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob <= min(null.prob) or
#>                      prob >= max(null.prob)
#>   H1 (Alternative) : prob  > min(null.prob) and
#>                      prob  < max(null.prob)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Target NCP (prob)    = 0.500 (vs. null.prob = 0.400 and 0.600)
#>   Number of Trials     = 200
#>   Type 1 Error (alpha) = 0.049
#>   Type 2 Error (beta)  = 0.229
#>   Statistical Power    = 0.771  <<
#> 
power.binom.test(power = 0.80, size = 200, req.sign = "0",
                 null.prob = c(0.4, 0.6), alpha = 0.05,
                 alternative = "two.one.sided")
#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE NCP CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Binomial Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : prob <= min(null.prob) or
#>                      prob >= max(null.prob)
#>   H1 (Alternative) : prob  > min(null.prob) and
#>                      prob  < max(null.prob)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Target NCP (prob)    = 0.500 (vs. null.prob = 0.400 and 0.600)  <<
#>   Number of Trials     = 200
#>   Type 1 Error (alpha) = 0.049
#>   Type 2 Error (beta)  = 0.229
#>   Statistical Power    = 0.771
#> 
```
