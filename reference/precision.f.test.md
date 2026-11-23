# Precision Calculation for the Generic F-Test

TBA...

## Usage

``` r
precision.f.test(statistic, df1, df2, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
```

## Arguments

- statistic:

  observed test statistic (F)

- df1:

  integer; numerator degrees of freedom.

- df2:

  integer; denominator degrees of freedom.

- probs:

  quantile probabilities, e.g., c(0.025, 0.975) for 95% CI (default:
  c(0.025, 0.25, 0.50, 0.75, 0.975))

## Value

- statistic:

  observed test statistic.

- df1:

  numerator degrees of freedom.

- df2:

  denominator degrees of freedom.

- probs:

  quantile probabilities.

- stat.at.probs:

  values for the test statistic at the defined probabilities.

## Examples

``` r
# TBA ...
```
