# Inflate Sample Size for Attrition

Helper function to inflate sample size for attrition.

## Usage

``` r
inflate.sample(n, rate = 0.05,
               ceiling = TRUE,
               verbose = TRUE)
```

## Arguments

- n:

  sample size.

- rate:

  attrition rate.

- ceiling:

  rounds-up the inflated sample size.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

## Value

inflated sample size.

## Examples

``` r
inflate.sample(n = 100, rate = 0.05)
#> 106
```
