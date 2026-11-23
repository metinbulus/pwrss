# Conversion from CI levels to probabilities

Calculates a probability vector (lower and upper threshold of the CI)
from a confidence interval (CI) level.

## Usage

``` r
ci.to.probs(ci.level = 0.95)
```

## Arguments

- ci.level:

  CI level (default: 0.95)

## Value

Probabilities (a numeric vector).

## Details

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `ci` instead of `ci.level`.

## Examples

``` r
ci.to.probs()        # c(0.025, 0.975)
#> [1] 0.025 0.975
ci.to.probs(0.99)    # c(0.005, 0.995)
#> [1] 0.005 0.995
ci.to.probs(0.9999)  # c(0.00005, 0.99995)
#> [1] 0.00005 0.99995
```
