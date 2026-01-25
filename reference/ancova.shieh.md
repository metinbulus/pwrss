# Power Analysis for One-, Two-, Three-Way ANCOVA Using Means, Standard Deviations, and (Optionally) Contrasts (F test)

Calculates power or sample size for one-, two-, three-way ANCOVA. For
factorial designs, use the argument `factor.levels` but note that unique
combination of levels (cells in this case) should follow a specific
order for the test of interaction. The order of marginal means and
standard deviations is printed as a warning message.

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `mu` or `mu.vec` instead of
`mu.vector`, or such as `k` or `k.cov` instead of `k.covariates`.

Formulas are validated using examples and tables in Shieh (2020).

## Usage

``` r
power.f.ancova.shieh(mu.vector, sd.vector,
                     n.vector = NULL, p.vector = NULL,
                     factor.levels = length(mu.vector),
                     r.squared = 0, k.covariates = 1,
                     contrast.matrix = NULL,
                     power = NULL, alpha = 0.05,
                     ceiling = TRUE, verbose = TRUE,
                     pretty = FALSE)
```

## Arguments

- mu.vector:

  vector; adjusted means (or estimated marginal means) for each level of
  a factor.

- sd.vector:

  vector; unadjusted standard deviations for each level of a factor. If
  a pooled standard deviation is provided, repeat its value to match the
  number of group means. A warning will be issued if group standard
  deviations differ substantially beyond what is expected due to
  sampling error.

- n.vector:

  vector; sample sizes for each level of a factor.

- p.vector:

  vector; proportion of total sample size in each level of a factor.
  These proportions should sum to one.

- factor.levels:

  integer; number of levels or groups in each factor. For example, for
  two factors each having two levels or groups use e.g. c(2, 2), for
  three factors each having two levels or groups use e.g. c(2, 2, 2).

- r.squared:

  explanatory power of covariates (R-squared) in the ANCOVA model.

- k.covariates:

  integer; number of covariates in the ANCOVA model.

- contrast.matrix:

  vector or matrix; contrasts should not be confused with the model
  (design) matrix. Rows of contrast matrix indicate independent vector
  of contrasts summing to zero. The default contrast matrix is
  constructed using deviation coding scheme (a.k.a. effect coding).
  Columns in the contrast matrix indicate number of levels or groups (or
  cells in factorial designs).

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

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

  type of the statistical test (F-Test)

- eta.squared:

  (partial) eta-squared.

- f:

  Cohen's f statistic.

- df1:

  numerator degrees of freedom.

- df2:

  denominator degrees of freedom.

- ncp:

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- power:

  statistical power \\(1-\beta)\\.

- n.total:

  total sample size.

## References

Shieh, G. (2020). Power analysis and sample size planning in ANCOVA
designs. Psychometrika, 85(1), 101-120.
[doi:10.1007/s11336-019-09692-3](https://doi.org/10.1007/s11336-019-09692-3)

## Examples

``` r
###################################################################
##########################  main effect  ##########################
###################################################################

# power for one-way ANCOVA (two levels or groups)
power.f.ancova.shieh(mu.vector = c(0.20, 0), # marginal means
                     sd.vector = c(1, 1), # unadjusted standard deviations
                     n.vector = c(150, 150), # sample sizes
                     r.squared = 0.50, # proportion of variance explained by covariates
                     k.covariates = 1, # number of covariates
                     alpha = 0.05)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
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
#>   Total Sample Size      = 300
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.316
#>   Statistical Power      = 0.684  <<
#> 


# sample size for one-way ANCOVA (two levels or groups)
power.f.ancova.shieh(mu.vector = c(0.20, 0), # marginal means
                     sd.vector = c(1, 1), # unadjusted standard deviations
                     p.vector = c(0.50, 0.50), # allocation, should sum to 1
                     r.squared = 0.50,
                     k.covariates = 1,
                     alpha = 0.05,
                     power = 0.80)
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
#>   Total Sample Size      = 396  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.199
#>   Statistical Power      = 0.801
#> 

###################################################################
#######################  interaction effect  ######################
###################################################################

# sample size for two-way ANCOVA (2 x 2)
power.f.ancova.shieh(mu.vector = c(0.20, 0.25, 0.15, 0.05), # marginal means
                     sd.vector = c(1, 1, 1, 1), # unadjusted standard deviations
                     p.vector = c(0.25, 0.25, 0.25, 0.25), # allocation, should sum to 1
                     factor.levels = c(2, 2), # 2 by 2 factorial design
                     r.squared = 0.50,
                     k.covariates = 1,
                     alpha = 0.05,
                     power = 0.80)
#> Elements of `mu.vector`, `sd.vector`, `n.vector` or `p.vector` should follow this specific order:
#> A1:B1  A1:B2  A2:B1  A2:B2  
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Two-way Analysis of Covariance (F-Test)
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
#>   Total Sample Size      = 2796  <<
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.200
#>   Statistical Power      = 0.8
#> 
# Elements of `mu.vector`, `sd.vector`, `n.vector` or `p.vector` should follow this specific order:
#  A1:B1  A1:B2  A2:B1  A2:B2

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
ancova.design <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), # marginal means
                                      sd.vector = c(1, 1, 1), # unadjusted standard deviations
                                      p.vector = c(1/3, 1/3, 1/3), # allocation, should sum to 1
                                      contrast.matrix = contrast.matrix,
                                      r.squared = 0.50,
                                      k.covariates = 1,
                                      alpha = 0.05,
                                      power = 0.80)
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
ancova.design <- power.f.ancova.shieh(mu.vector = c(0.15, 0.30, 0.20), # marginal means
                                      sd.vector = c(1, 1, 1), # unadjusted standard deviations
                                      p.vector = c(1/3, 1/3, 1/3), # allocation, should sum to 1
                                      contrast.matrix = contrast.matrix,
                                      r.squared = 0.50,
                                      k.covariates = 1,
                                      alpha = 0.05,
                                      power = 0.80)
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
                                      alpha = 0.05,
                                      power = 0.80)
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
                                      alpha = 0.05,
                                      power = 0.80)
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
