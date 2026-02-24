# Inflate Sample Size for Attrition

Inflate Sample Size for Attrition

## Usage

``` r
inflate.sample(n, rate = 0.05, ceiling = TRUE, verbose = 1)
```

## Arguments

- n:

  sample size.

- rate:

  attrition rate.

- ceiling:

  rounds-up the inflated sample size.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

inflated sample size.

## Examples

``` r
inflate.sample(n = 100, rate = 0.05)
#> 106
```
