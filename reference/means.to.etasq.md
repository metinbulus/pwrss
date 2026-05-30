# Conversion from Means and Standard Deviations to Cohen's f and Eta-squared

Calculates Cohen's f or Eta-squared for one-way ANOVA/ANCOVA. Set
`k.cov = 0` for one-way ANOVA (without any pretest or covariate
adjustment). Set `k.cov > 0` in combination with `r.squared > 0` for
one-way ANCOVA (with pretest or covariate adjustment).

## Usage

``` r
means.to.etasq(
  mu.vector,
  sd.vector,
  n.vector,
  k.covariates = 0,
  r.squared = 0,
  factor.levels = NULL,
  verbose = 1
)
```

## Arguments

- mu.vector:

  vector of adjusted means (or estimated marginal means) for each level
  of a factor.

- sd.vector:

  vector of unadjusted standard deviations for each level of a factor.

- n.vector:

  vector of sample sizes for each level of a factor.

- k.covariates:

  integer; number of covariates in the ANCOVA model. The default is
  `k.covariates = 0`, which means an ANOVA model would be of interest.

- r.squared:

  explanatory power of covariates (R-squared) in the ANCOVA model. The
  default is `r.squared = 0`, which means an ANOVA model would be of
  interest.

- factor.levels:

  integer; number of levels or groups in each factor. For example, for
  two factors each having two levels or groups use e.g. c(2, 2), for
  three factors each having two levels or groups use e.g. c(2, 2, 2)

- verbose:

  `1` by default (returns results), if `0` no output is printed on the
  console.

## Value

- f:

  Cohen's f

- eta.squared:

  (partial) eta-squared.

- df1:

  numerator degrees of freedom.

- df2:

  denominator degrees of freedom.

- ncp:

  non-centrality parameter under alternative.

## Details

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `mu` or `mu.vec` instead of
`mu.vector`, or such as `k` or `k.cov` instead of `k.covariates`.

## References

Keppel, G., & Wickens, T. D. (2004). Design and analysis: A researcher's
handbook (4th ed.). Pearson.

## Examples

``` r
means.to.etasq(mu.vector = c(0.50, 0), # marginal means
               sd.vector = c(1, 1), # unadjusted standard deviation
               n.vector = c(33, 33), # sample size (will be calculated)
               k.cov = 1, # number of covariates
               r.squared = 0.50)
#>           f eta.squared         df1         df2         ncp 
#>   0.3535534   0.1111111   1.0000000  63.0000000   8.2500000 
```
