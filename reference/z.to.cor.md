# Conversion from a z-value to a correlation (inverse Fisher's z-transformation)

Conversion from a z-value to a correlation (inverse Fisher's
z-transformation)

## Usage

``` r
z.to.cor(z, verbose = 1)
```

## Arguments

- z:

  z-value

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

- rho:

  correlation

- z:

  z-value

## Examples

``` r
z.to.cor(z = 0.1003353)
#>        rho          z 
#> 0.09999995 0.10033530 
z.to.cor(z = 0.5493061)
#>       rho         z 
#> 0.5000000 0.5493061 
z.to.cor(z = 1.4722193)
#>      rho        z 
#> 0.900000 1.472219 
z.to.cor(z = 2.6466524)
#>      rho        z 
#> 0.990000 2.646652 
```
