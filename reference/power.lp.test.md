# Statistical Power for the Lambda-Prime Distribution

Determines the power, the non-centrality parameter, or the degrees of
freedom for the lambda-prime distribution with (optional) Type 1 and
Type 2 error plots.

## Usage

``` r
power.lp.test(
  power = NULL,
  ncp = NULL,
  req.sign = "+",
  null.ncp = 0,
  df = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  plot = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- power:

  statistical power \\(1 - \beta)\\; either `power`, `ncp` or `df` needs
  to be NULL (and is then estimated).

- ncp:

  non-centrality parameter for the alternative; either `power`, `ncp` or
  `df` needs to be NULL (and is then estimated).

- req.sign:

  whether `ncp` is expected to be greater '+1', less than '-1', or
  within '0' the `null.ncp` bounds; only relevant if `ncp` is to be
  estimated.

- null.ncp:

  non-centrality parameter for the null. When alternative =
  "two.one.sided", the function expects two values in the form
  `c(lower, upper)`. If a single value is provided, it is interpreted as
  the absolute bound and automatically expanded to `c(-value, +value)`.

- df:

  degrees of freedom; either `power`, `ncp` or `df` needs to be NULL
  (and is then estimated).

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". "two.one.sided" is used for
  equivalence and minimal effect testing.

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

- ncp:

  non-centrality parameter under alternative.

- ncp.null:

  non-centrality parameter under null.

- df:

  degrees of freedom.

- alpha:

  type 1 error rate (user-specified).

- alternative:

  the direction or type of the hypothesis test.

- t.alpha:

  critical value(s).

- beta:

  type 2 error rate.

- type.s:

  type S error rate (only for two-tailed test).

- type.m:

  type M error rate (only for two-tailed test).

## Examples

``` r
# two-sided
# power defined as the probability of observing test statistics greater
# than the positive critical value OR less than the negative critical value
power.lp.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "two.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp  = null.ncp
#>   H1 (Alternative) : ncp != null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 1.960 (vs. null.ncp = 0)
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.502
#>   Statistical Power    = 0.498  <<
#> 
power.lp.test(power = 0.80, df = 100, alpha = 0.05, alternative = "two.sided")

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE EFFECT CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp  = null.ncp
#>   H1 (Alternative) : ncp != null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 2.825 (vs. null.ncp = 0)  <<
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# one-sided
# power is defined as the probability of observing a test statistic greater
# than the critical value
power.lp.test(ncp = 1.96, df = 100, alpha = 0.05, alternative = "one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp <= null.ncp
#>   H1 (Alternative) : ncp  > null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 1.960 (vs. null.ncp = 0)
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.379
#>   Statistical Power    = 0.621  <<
#> 
power.lp.test(power = 0.80, df = 100, alpha = 0.05, alternative = "one.sided")

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE EFFECT CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp <= null.ncp
#>   H1 (Alternative) : ncp  > null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 2.506 (vs. null.ncp = 0)  <<
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# equivalence
# power is defined as the probability of observing a test statistic greater
# than the upper critical value (for the lower bound) AND less than the
# lower critical value (for the upper bound)
power.lp.test(ncp = 0, null.ncp = c(-2, 2), df = 100, alpha = 0.05,
              alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp <= min(null.ncp) or
#>                      ncp >= max(null.ncp)
#>   H1 (Alternative) : ncp  > min(null.ncp) and
#>                      ncp  < max(null.ncp)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 0 (vs. null.ncp = -2 and 2)
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.738
#>   Statistical Power    = 0.262  <<
#> 
power.lp.test(power = 0.80, req.sign = "0", null.ncp = c(-2, 2),
              df = 100, alpha = 0.05, alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE EFFECT CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp <= min(null.ncp) or
#>                      ncp >= max(null.ncp)
#>   H1 (Alternative) : ncp  > min(null.ncp) and
#>                      ncp  < max(null.ncp)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = -0.000 (vs. null.ncp = -2 and 2)  <<
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.738
#>   Statistical Power    = 0.262
#> 

# minimal effect testing
# power is defined as the probability of observing a test statistic greater
# than the upper critical value (for the upper bound) OR less than the lower
# critical value (for the lower bound).
power.lp.test(ncp = 2, null.ncp = c(-1, 1), df = 100, alpha = 0.05,
              alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp >= min(null.ncp) and
#>                      ncp <= max(null.ncp)
#>   H1 (Alternative) : ncp  < min(null.ncp) or
#>                      ncp  > max(null.ncp)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 2 (vs. null.ncp = -1 and 1)
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.831
#>   Statistical Power    = 0.169  <<
#> 
power.lp.test(power = 0.80, req.sign = "+", null.ncp = c(-1, 1),
              df = 100, alpha = 0.05, alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE EFFECT CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Lambda-Prime Distribution
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp >= min(null.ncp) and
#>                      ncp <= max(null.ncp)
#>   H1 (Alternative) : ncp  < min(null.ncp) or
#>                      ncp  > max(null.ncp)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 3.844 (vs. null.ncp = -1 and 1)  <<
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
