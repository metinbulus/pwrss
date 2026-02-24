# Conversion from a correlation to a z-value (Fisher's z-transformation)

Conversion from a correlation to a z-value (Fisher's z-transformation)

## Usage

``` r
cor.to.z(rho, verbose = 1)
```

## Arguments

- rho:

  correlation

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

- z:

  z-value

- rho:

  correlation

## Examples

``` r
cor.to.z(rho = 0.1)
#>         z       rho 
#> 0.1003353 0.1000000 
cor.to.z(rho = 0.5)
#>         z       rho 
#> 0.5493061 0.5000000 
cor.to.z(rho = 0.9)
#>        z      rho 
#> 1.472219 0.900000 
cor.to.z(rho = 0.99)
#>        z      rho 
#> 2.646652 0.990000 
```
