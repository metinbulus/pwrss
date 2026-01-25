# Power Analysis for One-, Two-, Three-Way ANCOVA Contrasts and Multiple Comparisons (T-Tests)

Calculates power or sample size for one-, two-, three-Way ANCOVA
contrasts and multiple comparisons. The `pwrss.t.contrast()` function
allows a single contrast. For multiple contrast testing (multiple
comparisons) use the `pwrss.t.contrasts()` function. The
`pwrss.t.contrasts()` function also allows adjustment to alpha due to
multiple testing. All arguments are the same as with the earlier
function except that it accepts an object returned from the
`pwrss.f.ancova.shieh()` function for convenience. Beware that, in this
case, all other arguments are ignored except `alpha` and `adjust.alpha`.

The `pwrss.t.contrasts()` function returns a data frame with as many
rows as number of contrasts and eight columns with names 'contrast',
'comparison', 'psi', 'd', 'ncp', 'df', 'n.total', and 'power'. 'psi' is
the contrast estimate. 'd' is the standardized contrast estimate.
Remaining parameters are explained elsewhere.

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `mu` or `mu.vec` instead of
`mu.vector`, or such as `k` or `k.cov` instead of `k.covariates`.

Formulas are validated using examples and tables in Shieh (2017).

## Usage

``` r
power.t.contrast(mu.vector,
                 sd.vector,
                 contrast.vector,
                 n.vector = NULL, p.vector = NULL,
                 r.squared = 0, k.covariates = 1,
                 power = NULL, alpha = 0.05,
                 tukey.kramer = FALSE,
                 ceiling = TRUE, verbose = TRUE, pretty = FALSE)

power.t.contrasts(x = NULL,
                  mu.vector = NULL,
                  sd.vector = NULL,
                  n.vector = NULL, p.vector = NULL,
                  r.squared = 0, k.covariates = 1,
                  contrast.matrix = NULL,
                  power = NULL, alpha = 0.05,
                  adjust.alpha = c("none", "tukey", "bonferroni",
                                   "holm", "hochberg", "hommel",
                                   "BH", "BY", "fdr"),
                  ceiling = TRUE, verbose = TRUE, pretty = FALSE)
```

## Arguments

- x:

  object; an object returned from `pwrss.f.ancova.shieh()` function.

- mu.vector:

  vector; adjusted means (or estimated marginal means) for each level of
  a factor. Ignored when 'x' is specified.

- sd.vector:

  vector; unadjusted standard deviations for each level of a factor.
  Ignored when 'x' is specified.

- n.vector:

  vector; sample sizes for each level of a factor. Ignored when 'x' is
  specified.

- p.vector:

  vector; proportion of total sample size in each level of a factor.
  These proportions should sum to one. Ignored when 'x' is specified.

- r.squared:

  explanatory power of covariates (R-squared) in the ANCOVA model.
  Ignored when 'x' is specified.

- k.covariates:

  Number of covariates in the ANCOVA model. Ignored when 'x' is
  specified.

- contrast.vector:

  vector; a single contrast in the form of a vector with as many
  elements as number of levels or groups (or cells in factorial
  designs). Ignored when 'x' is specified.

- contrast.matrix:

  vector or matrix; contrasts should not be confused with the model
  (design) matrix. Rows of contrast matrix indicate independent vector
  of contrasts summing to zero. The default contrast matrix is
  constructed using deviation coding scheme (a.k.a. effect coding).
  Columns in the contrast matrix indicate number of levels or groups (or
  cells in factorial designs). Ignored when 'x' is specified.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\. Ignored when 'x' is
  specified.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\. Note that this should
  be specified even if 'x' is specified. The 'alpha' value within the
  'x' object pertains to the omnibus test, NOT the test of contrasts.

- tukey.kramer:

  logical; `TRUE` by default. If `FALSE` no adjustment will be made to
  control Type 1 error.

- adjust.alpha:

  character; one of the methods in c("none", "tukey", "bonferroni",
  "holm", "hochberg", "hommel", "BH", "BY", "fdr") to control Type 1
  error. See
  [`?stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- ceiling:

  logical; `TRUE` by default. If `FALSE` sample size in each cell is NOT
  rounded up.

- verbose:

  logical; `TRUE` by default. If `FALSE` no output is printed on the
  console.

- pretty:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

- parms:

  list of parameters used in calculation.

- test:

  type of the statistical test (T-Test).

- psi:

  contrast-weighted mean difference.

- d:

  contrast-weighted standardized mean difference.

- df:

  degrees of freedom.

- t.alpha:

  critical values.

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- power:

  statistical power \\(1-\beta)\\.

- n.total:

  total sample size.

## References

Shieh, G. (2017). Power and sample size calculations for contrast
analysis in ANCOVA. Multivariate Behavioral Research, 52(1), 1-11.
[doi:10.1080/00273171.2016.1219841](https://doi.org/10.1080/00273171.2016.1219841)

## Examples

``` r
  ###################################################################
  #######################  planned contrasts  #######################
  ###################################################################

  #########################
  ## dummy coding scheme ##
  #########################

  contrast.object <- factorial.contrasts(factor.levels = 3, # one factor w/ 3 levels
                                         coding = "treatment") # use dummy coding scheme
#>    A1 A2 A3
#> A1  1  0 -1
#> A2  0  1 -1

  # get contrast matrix from the contrast object
  contrast.matrix <- contrast.object$contrast.matrix

  # calculate sample size given design characteristics
  ancova.design <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30,  0.20), # marginal means
                                        sd.vector = c(1, 1, 1), # unadjusted standard deviations
                                        p.vector = c(1/3, 1/3, 1/3), # allocation, should sum to 1
                                        contrast.matrix = contrast.matrix,
                                        r.squared = 0.50,
                                        k.covariates = 1,
                                        alpha = 0.05, power = 0.80)
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
#>   Total Sample Size      = 1245  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.199
#>   Statistical Power      = 0.801
#> 

  # power of planned contrasts, adjusted for alpha level
  power.t.contrasts(ancova.design, adjust.alpha = "fdr")
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Multiple Contrast Analyses (T-Tests)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : psi = 0 
#>   H1 (Alt. Claim) : psi != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>  contr comparison   psi      d    ncp n.total power
#>      1  A1 <=> A3 -0.05 -0.071 -1.018    1245 0.111
#>      2  A2 <=> A3  0.10  0.141  2.036    1245 0.418
#> 

  ###########################
  ## Helmert coding scheme ##
  ###########################

  contrast.object <- factorial.contrasts(factor.levels = 3, # one factor w/ 4 levels
                                         coding = "helmert") # use helmert coding scheme
#>        A1     A2    A3
#> A1 -0.500  0.500 0.000
#> A2 -0.167 -0.167 0.333

  # get contrast matrix from the contrast object
  contrast.matrix <- contrast.object$contrast.matrix

  # calculate sample size given design characteristics
  ancova.design <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30,  0.20), # marginal means
                                        sd.vector = c(1, 1, 1), # unadjusted standard deviations
                                        p.vector = c(1/3, 1/3, 1/3), # allocation, should sum to 1
                                        contrast.matrix = contrast.matrix,
                                        r.squared = 0.50,
                                        k.covariates = 1,
                                        alpha = 0.05, power = 0.80)
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
#>   Total Sample Size      = 1245  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.199
#>   Statistical Power      = 0.801
#> 

  # power of planned contrasts
  power.t.contrasts(ancova.design)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Multiple Contrast Analyses (T-Tests)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : psi = 0 
#>   H1 (Alt. Claim) : psi != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>  contr   comparison    psi      d    ncp n.total power
#>      1    A2 <=> A1  0.075  0.106  3.055    1245 0.863
#>      2 A3 <=> A1 A2 -0.008 -0.012 -0.588    1245 0.090
#> 

  ##############################
  ## polynomial coding scheme ##
  ##############################

  contrast.object <- factorial.contrasts(factor.levels = 3, # one factor w/ 4 levels
                                         coding = "poly") # use polynomial coding scheme
#>         A1     A2    A3
#> A.L -0.707  0.000 0.707
#> A.Q  0.408 -0.816 0.408

  # get contrast matrix from the contrast object
  contrast.matrix <- contrast.object$contrast.matrix

  # calculate sample size given design characteristics
  ancova.design <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), # marginal means
                                        sd.vector = c(1, 1, 1), # unadjusted standard deviations
                                        p.vector = c(1/3, 1/3, 1/3), # allocation, should sum to 1
                                        contrast.matrix = contrast.matrix,
                                        r.squared = 0.50,
                                        k.covariates = 1,
                                        alpha = 0.05, power = 0.80)
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
#>   Total Sample Size      = 1245  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.199
#>   Statistical Power      = 0.801
#> 

  # power of the planned contrasts
  power.t.contrasts(ancova.design)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Multiple Contrast Analyses (T-Tests)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : psi = 0 
#>   H1 (Alt. Claim) : psi != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>  contr   comparison    psi      d    ncp n.total power
#>      1    A3 <=> A1  0.035  0.050  1.018    1245 0.174
#>      2 A1 A3 <=> A2 -0.102 -0.144 -2.939    1245 0.836
#> 

  ######################
  ## custom contrasts ##
  ######################

  # custom contrasts
  contrast.matrix <- rbind(
    cbind(A1 = 1, A2 = -0.50, A3 = -0.50),
    cbind(A1 = 0.50, A2 = 0.50, A3 = -1)
  )
  # labels are not required for custom contrasts,
  # but they make it easier to understand power.t.contrasts() output

  # calculate sample size given design characteristics
  ancova.design <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), # marginal means
                                        sd.vector = c(1, 1, 1), # unadjusted standard deviations
                                        p.vector = c(1/3, 1/3, 1/3), # allocation, should sum to 1
                                        contrast.matrix = contrast.matrix,
                                        r.squared = 0.50,
                                        k.covariates = 1,
                                        alpha = 0.05, power = 0.80)
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
#>   Total Sample Size      = 1245  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.199
#>   Statistical Power      = 0.801
#> 

  # power of the planned contrasts
  power.t.contrasts(ancova.design)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Multiple Contrast Analyses (T-Tests)
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim) : psi = 0 
#>   H1 (Alt. Claim) : psi != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>  contr   comparison    psi      d    ncp n.total power
#>      1 A1 <=> A2 A3 -0.100 -0.141 -2.351    1245 0.652
#>      2 A1 A2 <=> A3  0.025  0.035  0.588    1245 0.090
#> 
```
