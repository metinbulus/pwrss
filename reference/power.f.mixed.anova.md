# Power Analysis for Mixed-Effects Analysis of Variance (F-Test)

Calculates power or sample size for mixed-effects ANOVA design with two
factors (between and within). When there is only one group observed over
time, this design is often referred to as repeated-measures ANOVA.

Formulas are validated using G\*Power and tables in the PASS
documentation.

## Usage

``` r
power.f.mixed.anova(
  eta.squared,
  null.eta.squared = 0,
  factor.levels = c(2, 2),
  factor.type = c("between", "within"),
  rho.within = 0.5,
  epsilon = 1,
  n.total = NULL,
  power = NULL,
  alpha = 0.05,
  effect = c("between", "within", "interaction"),
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

  vector; integer; length of two representing the number of levels for
  groups and measures. For example, in randomized controlled trials with
  two arms (treatment and control) where pre-test, post-test, and
  follow-up test are administered, this would be represented as c(2, 3).

- factor.type:

  vector; character; length of two indicating the order of
  between-subject and within-subject factors. By default, the first
  value represents the between-subject factor and the second value
  represents the within-subject factor. This argument is rarely needed,
  except when unsure which element in 'factor.levels' represent
  between-subject or within-subject factors. Therefore, specify the
  'factor.levels' accordingly.

- rho.within:

  Correlation between repeated measures. For example, for
  pretest/post-test designs, this is the correlation between pretest and
  post-test scores regardless of group membership. The default is 0.50.
  If `eta.squared` is already adjusted for this correlation specify
  `rho.within = NA`.

- epsilon:

  non-sphericity correction factor, default is 1 (means no violation of
  sphericity). Lower bound for this argument is
  `epsilon = 1 / (factor.levels[2] - 1)`.

- n.total:

  integer; total sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- effect:

  character; the effect of interest: "between", "within", or
  "interaction".

- ceiling:

  logical; `TRUE` by default. If `FALSE` sample size in each group is
  NOT rounded up.

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

  non-centrality parameter under alternative.

- null.ncp:

  non-centrality parameter under null.

- power:

  statistical power \\(1-\beta)\\.

- n.total:

  total sample size.

## Details

- NB: The `pwrss.f.rmanova()` function is deprecated and will no longer
  be supported, but it will remain available as a wrapper for the
  `power.f.mixed.anova()` function during a transition period.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

## Examples

``` r
######################################################
# pretest-post-test design with treatment group only  #
######################################################

# a researcher is expecting a difference of Cohen's d = 0.30
# between post-test and pretest score translating into
# Eta-squared = 0.022

# adjust effect size for correlation with 'rho.within'
power.f.mixed.anova(eta.squared = 0.022,
                    factor.levels = c(1, 2), # 1 between 2 within
                    rho.within = 0.50,
                    effect = "within",
                    power = 0.80, alpha = 0.05)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Repeated Measures Analysis of Variance (F-Test)
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
#>   Total Sample Size    = 90  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.196
#>   Statistical Power    = 0.804
#> 

# if effect size is already adjusted for correlation
# use 'rho.within = NA'
power.f.mixed.anova(eta.squared = 0.08255,
                    factor.levels = c(1, 2), # 1 between 2 within
                    rho.within = NA,
                    effect = "within",
                    power = 0.80, alpha = 0.05)
#> Warning: Assuming that `eta.squared` and `null.eta.squared` are already adjusted for within-subject correlation.
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Repeated Measures Analysis of Variance (F-Test)
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
#>   Total Sample Size    = 90  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.196
#>   Statistical Power    = 0.804
#> 

##########################################################
# post-test only design with treatment and control groups #
##########################################################

# a researcher is expecting a difference of Cohen's d = 0.50
# on the post-test score between treatment and control groups
# translating into Eta-squared = 0.059
power.f.mixed.anova(eta.squared = 0.059,
                    factor.levels = c(2, 1),  # 2 between 1 within
                    effect = "between",
                    power = 0.80, alpha = 0.05)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Analysis of Variance (F-Test)
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


#############################################################
# pretest-post-test design with treatment and control groups #
#############################################################

# a researcher is expecting a difference of Cohen's d = 0.40
# on the post-test score between treatment and control groups
# after controlling for the pretest translating into
# partial Eta-squared = 0.038
power.f.mixed.anova(eta.squared = 0.038,
                    factor.levels = c(2, 2),  # 2 between 2 within
                    rho.within = 0.50,
                    effect = "between",
                    power = 0.80, alpha = 0.05)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Mixed-Effects Analysis of Variance (F-Test)
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
#>   Total Sample Size    = 152  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.197
#>   Statistical Power    = 0.803
#> 

# a researcher is expecting an interaction effect
# (between groups and time) of Eta-squared = 0.01
power.f.mixed.anova(eta.squared = 0.01,
                    factor.levels = c(2, 2),  # 2 between 2 within
                    rho.within = 0.50,
                    effect = "interaction",
                    power = 0.80, alpha = 0.05)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Mixed-Effects Analysis of Variance (F-Test)
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
#>   Total Sample Size    = 198  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.196
#>   Statistical Power    = 0.804
#> 

# a researcher is expecting an interaction effect
# (between groups and time) of Eta-squared = 0.01
power.f.mixed.anova(eta.squared = 0.01,
                    factor.levels = c(2, 2),  # 2 between 2 within
                    rho.within = 0.50,
                    effect = "within",
                    power = 0.80, alpha = 0.05)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Mixed-Effects Analysis of Variance (F-Test)
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
#>   Total Sample Size    = 198  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.196
#>   Statistical Power    = 0.804
#> 
```
