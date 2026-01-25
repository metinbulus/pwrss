# Conversion from Correlation Difference to Cohen's q

Helper function to convert correlation difference to Cohen's q (and vice
versa). `cor.to.z()` function applies Fisher's z transformation.

## Usage

``` r
cor.to.z(rho, verbose = TRUE)

  cors.to.q(rho1, rho2, verbose = TRUE)

  q.to.cors(q, rho1 = NULL, rho2 = NULL, verbose = TRUE)
```

## Arguments

- rho:

  correlation.

- rho1:

  first correlation.

- rho2:

  second correlation.

- q:

  Cohen's q effect size.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

## Value

- rho1:

  first correlation.

- rho2:

  second correlation.

- delta:

  correlation difference: rho1 - rho2.

- q:

  Cohen's q effect size.

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
