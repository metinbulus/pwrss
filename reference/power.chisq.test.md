# Statistical Power for the Generic Chi-Square Test

Determines power, the non-centrality parameter (NCP) for the generic
chi-square test with (optional) Type 1 and Type 2 error plots.

## Usage

``` r
power.chisq.test(
  power = NULL,
  ncp = NULL,
  null.ncp = 0,
  df = NULL,
  alpha = 0.05,
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

- null.ncp:

  non-centrality parameter for the null.

- df:

  integer; degrees of freedom, e.g., for the test of independence df =
  (nrow - 1) \* (ncol - 1); either `power`, `ncp` or `df` needs to be
  NULL (and is then estimated).

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

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

  statistical power \\(1-\beta)\\.

- ncp:

  non-centrality parameter under alternative.

- null.ncp:

  non-centrality parameter under null.

- df:

  degrees of freedom.

- alpha:

  type 1 error rate (user-specified).

- chisq.alpha:

  critical value.

## Examples

``` r
# power is defined as the probability of observing a test statistics greater
# than the critical value
power.chisq.test(ncp = 20, df = 100, alpha = 0.05)

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic Chi-square Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp = null.ncp
#>   H1 (Alternative) : ncp > null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 20 (vs. null.ncp = 0)
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.619
#>   Statistical Power    = 0.381  <<
#> 
power.chisq.test(power = 0.80, df = 100, alpha = 0.05)

#> +--------------------------------------------------+
#> |      MINIMUM DETECTABLE EFFECT CALCULATION       |
#> +--------------------------------------------------+
#> 
#> Generic Chi-square Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp = null.ncp
#>   H1 (Alternative) : ncp > null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 40.556 (vs. null.ncp = 0)  <<
#>   Degrees of Freedom   = 100
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
power.chisq.test(power = 0.80, ncp = 20, alpha = 0.05)

#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Generic Chi-square Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : ncp = null.ncp
#>   H1 (Alternative) : ncp > null.ncp
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (ncp)    = 20 (vs. null.ncp = 0)
#>   Degrees of Freedom   = 17.673  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
