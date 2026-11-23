# Precision Calculation for the Generic Binomial Test

TBA...

## Usage

``` r
precision.binom.test(statistic, size, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
```

## Arguments

- statistic:

  observed test statistic, which is a proportion

- size:

  number of trials (zero or more)

- probs:

  quantile probabilities, e.g., c(0.025, 0.975) for 95% CI (default:
  c(0.025, 0.25, 0.50, 0.75, 0.975))

## Value

- statistic:

  observed test statistic.

- size:

  number of trials (zero or more).

- probs:

  quantile probabilities.

- stat.at.probs:

  values for the test statistic at the defined probabilities.

## References

Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or
fiducial limits illustrated in the case of the binomial. *Biometrika,
26*(4), 404–413. https://doi.org/10.2307/2331986

## Examples

``` r
# TBA ...
```
