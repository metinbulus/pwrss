# Power Analysis for Linear Regression: Single Coefficient (T-Test)

Calculates power or sample size (only one can be NULL at a time) to test
a single coefficient in multiple linear regression. The predictor is
assumed to be continuous by default. However, one can calculate power or
sample size for a binary predictor (such as treatment and control groups
in an experimental design) by specifying `sd.predictor = sqrt(p*(1-p))`
where `p` is the proportion of subjects in one of the groups. The sample
size in each group would be `n*p` and `n*(1-p)`.
`power.t.regression()``pwrss.t.regression()` are the same functions, as
well as `power.t.reg()` and `pwrss.t.reg()`.

Minimal effect and equivalence tests are implemented in line with Hodges
and Lehmann (1954), Kim and Robinson (2019), Phillips (1990), and Dupont
and Plummer (1998).

Formulas are validated using Monte Carlo simulation, G\*Power, tables in
PASS documentation, and tables in Bulus (2021).

NOTE: The `pwrss.t.regression()` function and its alias `pwrss.z.reg()`
are deprecated, but they will remain available as a wrapper for
`power.t.regression()` during the transition period.

## Usage

``` r
power.t.regression(beta, null.beta = 0, margin = 0,
                   sd.predictor = 1, sd.outcome = 1,
                   r.squared = (beta * sd.predictor / sd.outcome)^2,
                   k.total = 1, n = NULL, power = NULL, alpha = 0.05,
                   alternative = c("two.sided", "one.sided", "two.one.sided"),
                   ceiling = TRUE, verbose = TRUE, pretty = FALSE)
```

## Arguments

- beta:

  regression coefficient. One can use standardized regression
  coefficient, but should keep `sd.predictor = 1` and `sd.outcome = 1`
  or leave them out as they are default specifications.

- null.beta:

  regression coefficient under null hypothesis (typically zero). One can
  use standardized regression coefficient, but should keep
  `sd.predictor = 1` and `sd.outcome = 1` or leave them out as they are
  default specifications.

- margin:

  margin - ignorable `beta` - `null.beta` difference.

- sd.predictor:

  standard deviation of the predictor. For a binary predictor,
  `sd.predictor = sqrt(p * (1 - p))` where `p` is the proportion of
  subjects in one of the groups.

- sd.outcome:

  standard deviation of the outcome.

- k.total:

  integer; total number of predictors, including the predictor of
  interest.

- r.squared:

  model R-squared. The default is
  `r.squared = (beta * sd.predictor / sd.outcome)^2` assuming a linear
  regression with one predictor. Thus, an `r.squared` below this value
  will throw a warning. To consider other covariates in the model
  provide a value greater than the default `r.squared` along with the
  argument `k.total > 1`.

- n:

  integer; sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided".

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

  type of the statistical test (T-Test).

- df:

  degrees of freedom.

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- t.alpha:

  critical value(s).

- power:

  statistical power \\(1-\beta)\\.

- n:

  sample size.

## References

Bulus, M. (2021). Sample size determination and optimal design of
randomized/non-equivalent pretest-post-test control-group designs.
Adiyaman University Journal of Educational Sciences, 11(1), 48-69.
[doi:10.17984/adyuebd.941434](https://doi.org/10.17984/adyuebd.941434)

Hodges Jr, J. L., & Lehmann, E. L. (1954). Testing the approximate
validity of statistical hypotheses. Journal of the Royal Statistical
Society Series B: Statistical Methodology, 16(2), 261-268.
[doi:10.1111/j.2517-6161.1954.tb00169.x](https://doi.org/10.1111/j.2517-6161.1954.tb00169.x)

Kim, J. H., & Robinson, A. P. (2019). Interval-based hypothesis testing
and its applications to economics and finance. Econometrics, 7(2), 21.
[doi:10.3390/econometrics7020021](https://doi.org/10.3390/econometrics7020021)

Phillips, K. F. (1990). Power of the two one-sided tests procedure in
bioequivalence. Journal of Pharmacokinetics and Biopharmaceutics, 18(2),
137-144. [doi:10.1007/bf01063556](https://doi.org/10.1007/bf01063556)

Dupont, W. D., and Plummer, W. D. (1998). Power and sample size
calculations for studies involving linear regression. Controlled
Clinical Trials, 19(6), 589-601.
[doi:10.1016/s0197-2456(98)00037-3](https://doi.org/10.1016/s0197-2456%2898%2900037-3)

## Examples

``` r
# continuous predictor x (and 4 covariates)
power.t.regression(beta = 0.20,
            k.total = 5,
            r.squared = 0.30,
            power = 0.80)
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
#>   Sample Size            = 140  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error           = 0.198
#>   Statistical Power      = 0.802
#> 

# binary predictor x (and 4 covariates)
p <- 0.50 # proportion of subjects in one group
power.t.regression(beta = 0.20,
            sd.predictor = sqrt(p*(1-p)),
            k.total = 5,
            r.squared = 0.30,
            power = 0.80)
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
#>   Sample Size            = 552  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error           = 0.200
#>   Statistical Power      = 0.8
#> 

# non-inferiority test with binary predictor x (and 4 covariates)
p <- 0.50 # proportion of subjects in one group
power.t.regression(beta = 0.20, # Cohen's d
            margin = -0.05, # non-inferiority margin in Cohen's d unit
            alternative = "one.sided",
            sd.predictor = sqrt(p*(1-p)),
            k.total = 5,
            r.squared = 0.30,
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression Coefficient (T-Test)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : beta - null.beta <= margin 
#>   H1 (Alt. Claim) : beta - null.beta >  margin 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size            = 278  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error           = 0.200
#>   Statistical Power      = 0.8
#> 

# superiority test with binary predictor x (and 4 covariates)
p <- 0.50 # proportion of subjects in one group
power.t.regression(beta = 0.20, # Cohen's d
            margin = 0.05, # superiority margin in Cohen's d unit
            alternative = "one.sided",
            sd.predictor = sqrt(p*(1-p)),
            k.total = 5,
            r.squared = 0.30,
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression Coefficient (T-Test)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : beta - null.beta <= margin 
#>   H1 (Alt. Claim) : beta - null.beta >  margin 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size            = 773  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error           = 0.200
#>   Statistical Power      = 0.8
#> 

# equivalence test with binary predictor x (and 4 covariates)
p <- 0.50 # proportion of subjects in one group
power.t.regression(beta = 0, # Cohen's d
            margin = c(-0.05, 0.05), # equivalence bounds in Cohen's d unit
            alternative = "two.one.sided",
            sd.predictor = sqrt(p*(1 - p)),
            k.total = 5,
            r.squared = 0.30,
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression Coefficient (T-Test)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : beta - null.beta <= min(margin) or 
#>                     beta - null.beta >= max(margin) 
#>   H1 (Alt. Claim) : beta - null.beta > min(margin) and 
#>                     beta - null.beta < max(margin)
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size            = 9593  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error           = 0.200
#>   Statistical Power      = 0.8
#> 
```
