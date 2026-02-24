# Statistical Power for the Generic Chi-Square Test

Calculates power for the generic chi-square test with (optional) Type 1
and Type 2 error plots.

## Usage

``` r
power.chisq.test(
  ncp,
  null.ncp = 0,
  df,
  alpha = 0.05,
  plot = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- df:

  integer; degrees of freedom. For example, for the test of independence
  df = (nrow - 1)\*(ncol - 1).

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
#>   H0 (Null)        : ncp = ncp.null
#>   H1 (Alternative) : ncp > ncp.null
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.619
#>   Statistical Power    = 0.381  <<
#> 
```
