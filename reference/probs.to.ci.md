# Conversion from probabilities to CI levels

Calculates a confidence interval (CI) level from a probability vector
(with the lower and upper threshold of the CI).

## Usage

``` r
probs.to.ci(probs = c(0.025, 0.975))
```

## Arguments

- probs:

  vector of probabilities to be converted into a CI level (default:
  c(0.025, 0.975)).

## Value

CI level (a numerical value)

## Details

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `p` or `prob` instead of
`probs`. Please note that the conversion back takes the minimum and
maximum value in `probs`. This permits you to submit vectors like those
shown in the examples.

## Examples

``` r
probs.to.ci(c(0.025, 0.5, 0.975))                 # 0.95
#> [1] 0.95
probs.to.ci(c(0.005, 0.025, 0.5, 0.975, 0.995))   # 0.99
#> [1] 0.99
probs.to.ci(c(0.025, 0.975, 0.5, 0.005, 0.995))   # 0.99
#> [1] 0.99
# probs.to.ci(c(0.005, 0.025, 0.5, 0.975, 0.9995))  # -> would throw an error, since 0.005 != (1 - 0.9995)
```
