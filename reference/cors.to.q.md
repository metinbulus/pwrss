# Conversion from a correlation Difference to Cohen's q

Conversion from a correlation Difference to Cohen's q

## Usage

``` r
cors.to.q(rho1, rho2, verbose = 1)
```

## Arguments

- rho1:

  first correlation.

- rho2:

  second correlation.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

- q:

  Cohen's q effect size.

- delta:

  correlation difference: rho1 - rho2.

- rho1:

  first correlation.

- rho2:

  second correlation.

## Examples

``` r
cors.to.q(rho2 = 0.5712027, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#> -0.1000000 -0.0712027  0.5000000  0.5712027 
cors.to.q(rho2 = 0.6907068, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#> -0.3000000 -0.1907068  0.5000000  0.6907068 
cors.to.q(rho2 = 0.7815365, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#> -0.5000001 -0.2815365  0.5000000  0.7815365 
```
