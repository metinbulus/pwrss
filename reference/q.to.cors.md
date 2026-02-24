# Conversion from a Cohen's q to a correlation difference

Conversion from a Cohen's q to a correlation difference

## Usage

``` r
q.to.cors(q, rho1 = NULL, rho2 = NULL, verbose = 1)
```

## Arguments

- q:

  Cohen's q effect size.

- rho1:

  first correlation (either rho1 or rho2 needs to be given)

- rho2:

  second correlation (either rho1 or rho2 needs to be given)

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
q.to.cors(q = 0.10, rho1 = 0.50)
#>           q       delta        rho1        rho2 
#>  0.10000000 -0.07120268  0.50000000  0.57120268 
q.to.cors(q = 0.30, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#>  0.3000000 -0.1907068  0.5000000  0.6907068 
q.to.cors(q = 0.50, rho1 = 0.50)
#>          q      delta       rho1       rho2 
#>  0.5000000 -0.2815365  0.5000000  0.7815365 
```
