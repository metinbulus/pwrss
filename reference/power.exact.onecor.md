# Power Analysis for One-Sample Correlation (Exact)

Calculates power, sample size, or minimum detectable correlation (only
one can be NULL at a time) to test a (Pearson) correlation against a
constant using exact method described in Barabesi and Greco (2002).

Formulas are validated using G\*Power.

## Usage

``` r
power.exact.onecor(
  rho = NULL,
  req.sign = "+",
  null.rho = 0,
  n = NULL,
  n.max = 10000,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- rho:

  correlation.

- req.sign:

  whether estimated rho is smaller or larger than the null.rho (when
  minimum detectable rho is of interest).

- null.rho:

  correlation when null is true. Only 0 is allowed for now.

- n:

  sample size.

- n.max:

  max. number of observations in the sample (default: 500).

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided"
  or "one.sided".

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

  type of the statistical test (Z-Test)

- rho.alpha:

  critical value(s).

- es:

  minimum detectable correlation.

- power:

  statistical power\\(1-\beta)\\.

- n:

  sample size.

## References

Barabesi, L., & Greco, L. (2002). A Note on the Exact Computation of the
Student t, Snedecor F and Sample Correlation Coefficient Distribution
Functions. Journal of the Royal Statistical Society. Series D (The
Statistician), 51(1), 105–110. https://www.jstor.org/stable/3650394

## Examples

``` r
# expected correlation is 0.20 and it is different from 0
# it could be 0.20 as well as -0.20
power.exact.onecor(rho = 0.20,
                   power = 0.80,
                   alpha = 0.05,
                   alternative = "two.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-Sample Correlation (Exact)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : rho - null.rho  = 0
#>   H1 (Alternative) : rho - null.rho != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (rho)    = 0.200 (vs. null.rho = 0)
#>   Sample Size          = 193  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# expected correlation is 0.20 and it is greater than 0
power.exact.onecor(rho = 0.20,
                   power = 0.80,
                   alpha = 0.05,
                   alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-Sample Correlation (Exact)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : rho - null.rho <= 0
#>   H1 (Alternative) : rho - null.rho  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Effect Size (rho)    = 0.200 (vs. null.rho = 0)
#>   Sample Size          = 153  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.198
#>   Statistical Power    = 0.802
#> 

```
