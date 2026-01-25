# Statistical Power for the Generic F-Test

Calculates power for the generic F-Test with (optional) Type 1 and Type
2 error plots.

## Usage

``` r
power.f.test(ncp, null.ncp = 0, df1, df2, alpha = 0.05,
             plot = TRUE, verbose = TRUE, pretty = FALSE)
```

## Arguments

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- df1:

  integer; numerator degrees of freedom.

- df2:

  integer; denominator degrees of freedom.

- plot:

  logical; `FALSE` switches off Type 1 and Type 2 error plot. `TRUE` by
  default.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

- pretty:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- df1:

  numerator degrees of freedom.

- df2:

  denominator degrees of freedom.

- ncp:

  non-centrality parameter under alternative.

- ncp.null:

  non-centrality parameter under null.

- f.alpha:

  critical value(s).

- power:

  statistical power \\(1-\beta)\\.

## Examples

``` r
# power is defined as the probability of observing F-statistics
# greater than the critical value
power.f.test(ncp = 1, df1 = 4, df2 = 100, alpha = 0.05)

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic F-Test
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : ncp = null.ncp 
#>   H1 (Alt. Claim) : ncp > null.ncp 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.897
#>   Statistical Power      = 0.103  <<
#> 
```
