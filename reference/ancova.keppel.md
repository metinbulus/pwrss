# Power Analysis for One-Way ANOVA/ANCOVA Using Means and Standard Deviations (F test)

Calculates power or sample size for one-way ANOVA/ANCOVA. Set
`k.cov = 0` for one-way ANOVA (without any pretest or covariate
adjustment). Set `k.cov > 0` in combination with `r2 > 0` for one-way
ANCOVA (with pretest or covariate adjustment).

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `mu` or `mu.vec` instead of
`mu.vector`, or such as `k` or `k.cov` instead of `k.covariates`.

Formulas are validated using PASS documentation.

## Usage

``` r
power.f.ancova.keppel(mu.vector, sd.vector,
                      n.vector = NULL, p.vector = NULL,
                      factor.levels = length(mu.vector),
                      r.squared = 0, k.covariates = 0,
                      power = NULL, alpha = 0.05,
                      ceiling = TRUE, verbose = TRUE,
                      pretty = FALSE)
```

## Arguments

- mu.vector:

  vector of adjusted means (or estimated marginal means) for each level
  of a factor.

- sd.vector:

  vector of unadjusted standard deviations for each level of a factor.

- n.vector:

  vector of sample sizes for each level of a factor.

- p.vector:

  vector of proportion of total sample size in each level of a factor.
  These proportions should sum to one.

- factor.levels:

  integer; number of levels or groups in each factor. For example, for
  two factors each having two levels or groups use e.g. c(2, 2), for
  three factors each having two levels or groups use e.g. c(2, 2, 2)

- r.squared:

  explanatory power of covariates (R-squared) in the ANCOVA model. The
  default is `r.squared = 0`, which means an ANOVA model would be of
  interest.

- k.covariates:

  integer; number of covariates in the ANCOVA model. The default is
  `k.cov = 0`, which means an ANOVA model would be of interest.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- ceiling:

  logical; whether sample size should be rounded up. `TRUE` by default.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

- pretty:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- parms:

  list of parameters used in calculation.

- test:

  type of the statistical test (F-Test).

- df1:

  numerator degrees of freedom.

- df2:

  denominator degrees of freedom.

- ncp:

  non-centrality parameter under alternative.

- null.ncp:

  non-centrality parameter under null.

- power:

  statistical power \\(1-\beta)\\.

- n.total:

  total sample size.

## References

Keppel, G., & Wickens, T. D. (2004). Design and analysis: A researcher's
handbook (4th ed.). Pearson.

## Examples

``` r
# required sample size to detect a mean difference of
# Cohen's d = 0.50 for a one-way two-group design
power.f.ancova.keppel(mu.vector = c(0.50, 0), # marginal means
                      sd.vector = c(1, 1), # unadjusted standard deviations
                      n.vector = NULL, # sample size (will be calculated)
                      p.vector = c(0.50, 0.50), # balanced allocation
                      k.cov = 1, # number of covariates
                      r.squared = 0.50, # explanatory power of covariates
                      alpha = 0.05, # Type 1 error rate
                      power = .80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-way Analysis of Covariance (F-Test)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : eta.squared = 0 
#>   H1 (Alt. Claim) : eta.squared > 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Total Sample Size      = 66  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.193
#>   Statistical Power      = 0.807
#> 

# effect size approach
power.f.ancova(eta.squared = 0.111, # effect size that is already adjusted for covariates
               factor.levels = 2, # one-way ANCOVA with two levels (groups)
               k.covariates = 1, # number of covariates
               alpha = 0.05, # Type 1 error rate
               power = .80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-way Analysis of Covariance (F-Test)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : eta.squared = 0 
#>   H1 (Alt. Claim) : eta.squared > 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Total Sample Size      = 66  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.193
#>   Statistical Power      = 0.807
#> 

# regression approach
p <- 0.50
power.t.regression(beta = 0.50,
                   sd.predictor = sqrt(p * (1 - p)),
                   sd.outcome = 1,
                   k.total = 1,
                   r.squared = 0.50,
                   n = NULL, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression Coefficient (T-Test)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : beta - null.beta = 0 
#>   H1 (Alt. Claim) : beta - null.beta != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size            = 65  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error           = 0.199
#>   Statistical Power      = 0.801
#> 
```
