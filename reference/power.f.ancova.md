# Power Analysis for One-, Two-, Three-Way ANOVA/ANCOVA Using Effect Size (F-Test)

Calculates power or sample size for one-way, two-way, or three-way
ANOVA/ANCOVA. Set `k.cov = 0` for ANOVA, and `k.cov > 0` for ANCOVA.
Note that in the latter, the effect size (`eta.squared` should be
obtained from the relevant ANCOVA model, which is already adjusted for
the explanatory power of covariates (thus, an additional R-squared
argument is not required as an input).

Formulas are validated using G\*Power and tables in the PASS
documentation.

## Usage

``` r
power.f.ancova(
  eta.squared,
  null.eta.squared = 0,
  factor.levels = 2,
  target.effect = NULL,
  k.covariates = 0,
  n.total = NULL,
  power = NULL,
  alpha = 0.05,
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- eta.squared:

  (partial) eta-squared for the alternative.

- null.eta.squared:

  (partial) eta-squared for the null.

- factor.levels:

  integer; number of levels or groups in each factor. For example, for
  two factors each having two levels or groups use e.g. c(2, 2), for
  three factors each having two levels (groups) use e.g. c(2, 2, 2).

- target.effect:

  character; determine the main effect or interaction that is of
  interest, e.g., in a three-way-design, it is possible to define "A"
  (main effect of the first factor), "B:C" (interaction of factor two
  and three) or "A:B:C" (the three-way interaction of all factors); if
  target is not used, the three-way interaction is the default.

- k.covariates:

  integer; number of covariates in an ANCOVA model

- n.total:

  integer; total sample size

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- ceiling:

  logical; if `FALSE` sample size in each cell is not rounded up.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

- utf:

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

  non-centrality parameter for the alternative.

- null.ncp:

  non-centrality parameter for the null.

- f.alpha:

  critical value.

- power:

  statistical power \\(1-\beta)\\.

- n.total:

  total sample size.

## Details

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `mu` or `mu.vec` instead of
`mu.vector`, or such as `k` or `k.cov` instead of `k.covariates`.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
#############################################
#              one-way ANOVA                #
#############################################

# Cohen's d = 0.50 between treatment and control
# translating into Eta-squared = 0.059

# estimate sample size using ANOVA approach
power.f.ancova(eta.squared = 0.059,
               factor.levels = 2,
               alpha = 0.05, power = .80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-way Analysis of Variance (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : eta.squared = 0
#>   H1 (Alternative) : eta.squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Total Sample Size    = 128  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.197
#>   Statistical Power    = 0.803
#> 

# estimate sample size using regression approach(F-Test)
power.f.regression(r.squared = 0.059,
                   k.total = 1,
                   alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : R-squared = 0
#>   H1 (Alternative) : R-squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 128  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.197
#>   Statistical Power    = 0.803
#> 

# estimate sample size using regression approach (T-Test)
p <- 0.50 # proportion of sample in treatment (allocation rate)
power.t.regression(beta = 0.50, r.squared = 0,
                   k.total = 1,
                   sd.predictor = sqrt(p*(1-p)),
                   alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Linear Regression Coefficient (T-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : beta - null.beta  = 0
#>   H1 (Alternative) : beta - null.beta != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 128  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

# estimate sample size using t test approach
power.t.student(d = 0.50, alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Student's T-Test (Independent Samples)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d  = 0
#>   H1 (Alternative) : d - null.d != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 64 and 64  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

#############################################
#              two-way ANOVA                #
#############################################

# a researcher is expecting a partial Eta-squared = 0.03
# for interaction of treatment (Factor A) with
# gender consisting of two levels (Factor B)

power.f.ancova(eta.squared = 0.03,
               factor.levels = c(2,2),
               alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Two-way Analysis of Variance (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : eta.squared = 0
#>   H1 (Alternative) : eta.squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Total Sample Size    = 256  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# estimate sample size using regression approach (F test)
# one dummy for treatment, one dummy for gender, and their interaction (k = 3)
# partial Eta-squared is equivalent to the increase in R-squared by adding
# only the interaction term (m = 1)
power.f.regression(r.squared = 0.03,
                   k.total = 3, k.test = 1,
                   alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Hierarchical Linear Regression (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Change in R-squared = 0
#>   H1 (Alternative) : Change in R-squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 256  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

#############################################
#              one-way ANCOVA               #
#############################################

# a researcher is expecting an adjusted difference of
# Cohen's d = 0.45 between treatment and control after
# controllling for the pretest (k.cov = 1)
# translating into Eta-squared = 0.048

power.f.ancova(eta.squared = 0.048,
               factor.levels = 2,
               k.covariates = 1,
               alpha = 0.05, power = .80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-way Analysis of Covariance (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : eta.squared = 0
#>   H1 (Alternative) : eta.squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Total Sample Size    = 158  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

#############################################
#              two-way ANCOVA               #
#############################################

# a researcher is expecting an adjusted partial Eta-squared = 0.02
# for interaction of treatment (Factor A) with
# gender consisting of two levels (Factor B)

power.f.ancova(eta.squared = 0.02,
               factor.levels = c(2,2),
               k.covariates = 1,
               alpha = 0.05, power = .80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Two-way Analysis of Covariance (F-Test)
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : eta.squared = 0
#>   H1 (Alternative) : eta.squared > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Total Sample Size    = 388  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 
```
