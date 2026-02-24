# Power Analysis for Logistic Regression Coefficient (Wald's Z-Test)

Calculates power or sample size (only one can be NULL at a time) to test
a single coefficient in logistic regression. `power.z.logistic()` and
`power.z.logreg()` are the same functions, as well as
`pwrss.z.logistic()` and `pwrss.z.logreg()`.

The distribution of the predictor variable can be one of the following:
`c("normal", "poisson", "uniform", "exponential", "binomial", "bernouilli", "lognormal")`
for Demidenko (2007) procedure but only
`c("normal", "binomial", "bernouilli")` for Hsieh et al. (1998)
procedure. The default parameters for these distributions are:

`distribution = list(dist = "normal", mean = 0, sd = 1)`  
`distribution = list(dist = "poisson", lambda = 1)`  
`distribution = list(dist = "uniform", min = 0, max = 1)`  
`distribution = list(dist = "exponential", rate = 1)`  
`distribution = list(dist = "binomial", size = 1, prob = 0.50)`  
`distribution = list(dist = "bernoulli", prob = 0.50)`  
`distribution = list(dist = "lognormal", meanlog = 0, sdlog = 1)`  

Parameters defined in [`list()`](https://rdrr.io/r/base/list.html) form
can be modified, but element names should be kept the same. It is
sufficient to use distribution's name for default parameters (e.g.
`dist = "normal"`).

Formulas are validated using G\*Power and tables in the PASS
documentation.

## Usage

``` r
power.z.logistic(
  prob = NULL,
  base.prob = NULL,
  odds.ratio = NULL,
  beta0 = NULL,
  beta1 = NULL,
  n = NULL,
  power = NULL,
  r.squared.predictor = 0,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
  method = c("demidenko(vc)", "demidenko", "hsieh"),
  distribution = "normal",
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- prob:

  probability under alternative hypothesis (probability that an event
  occurs when the value of the predictor is increased from 0 to 1).
  Warning: This is base probability + incremental increase.

- base.prob:

  base probability under null hypothesis (probability that an event
  occurs without the influence of the predictor - or when the value of
  the predictor is zero).

- odds.ratio:

  odds ratio defined as
  `odds.ratio = exp(beta1) = (prob / (1 - prob)) / (base.prob / (1 - base.prob))`

- beta0:

  regression coefficient defined as
  `beta0 = log(base.prob/(1-base.prob))`

- beta1:

  regression coefficient for the predictor X defined as
  `beta1 = log((prob / (1 - prob)) / (base.prob / (1 - base.prob)))`

- n:

  integer; sample size

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- r.squared.predictor:

  proportion of variance in the predictor accounted for by other
  covariates. This is not a pseudo R-squared. To compute it, regress the
  predictor on the covariates and extract the adjusted R-squared from
  that model.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided"
  or "one.sided".

- method:

  character; analytic method. `"demidenko(vc)"` stands for
  Demidenko (2007) procedure with variance correction; `"demidenko"`
  stands for Demidenko (2007) procedure without variance correction;
  `"hsieh"` stands for Hsieh et al. (1998) procedure. `"demidenko"` and
  `"hsieh"` methods produce similar results but `"demidenko(vc)"` is
  more precise.

- distribution:

  character; distribution family. Can be one of the
  `c("normal", "poisson", "uniform", "exponential", "binomial", "bernouilli", "lognormal")`
  for Demidenko (2007) procedure but only
  `c("normal", "binomial", "bernouilli")` for the Hsieh et al. (1998)
  procedure.

- ceiling:

  logical; whether sample size should be rounded up. `TRUE` by default.

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

  type of the statistical test (Z-Test).

- mean:

  mean of the alternative distribution.

- sd:

  standard deviation of the alternative distribution.

- null.mean:

  mean of the null distribution.

- null.sd:

  standard deviation of the null distribution.

- z.alpha:

  critical value(s).

- power:

  statistical power \\(1-\beta)\\.

- n:

  sample size.

## Details

- NB: The `pwrss.z.logistic()` and its alias `pwrss.z.logreg()` are
  deprecated. However, they will remain available as wrappers for the
  `power.z.logistic()` function during a transition period.

## References

Demidenko, E. (2007). Sample size determination for logistic regression
revisited. *Statistics in Medicine, 26*(18), 3385-3397.
https://doi.org/10.1002/sim.2771

Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of
sample size calculation for linear and logistic regression. *Statistics
in Medicine, 17*(4), 1623-1634.

## Examples

``` r
###########################################
# predictor X follows normal distribution #
###########################################

## probability specification
power.z.logistic(base.prob = 0.15, prob = 0.20,
                 alpha = 0.05, power = 0.80,
                 dist = "normal")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 511  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## odds ratio specification
power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
                 alpha = 0.05, power = 0.80,
                 dist = "normal")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 511  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## regression coefficient specification
power.z.logistic(beta0 = -1.734601, beta1 = 0.3483067,
                 alpha = 0.05, power = 0.80,
                 dist = "normal")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 511  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## change parameters associated with predictor X
pred.dist <- list(dist = "normal", mean = 10, sd = 2)
power.z.logistic(base.prob = 0.15, beta1 = 0.3483067,
                 alpha = 0.05, power = 0.80,
                 dist = pred.dist)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 134  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 


##############################################
# predictor X follows Bernoulli distribution #
# (such as treatment/control groups)         #
##############################################

## odds ratio specification
power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
                 alpha = 0.05, power = 0.80,
                 dist = "bernoulli")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Bernoulli
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 1816  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## change parameters associated with predictor X
pred.dist <- list(dist = "bernoulli", prob = 0.30)
power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
                 alpha = 0.05, power = 0.80,
                 dist = pred.dist)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Bernoulli
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 2114  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

####################################
# predictor X is an ordinal factor #
####################################

## generating an ordinal predictor
x.ord <- sample(
  x = c(1, 2, 3, 4), # levels
  size = 1e5, # sample size large enough to get stable estimates
  prob = c(0.25, 0.25, 0.25, 0.25), # category probabilities
  replace = TRUE
)

## dummy coding the ordinal predictor
x.ord <- factor(x.ord, ordered = TRUE)
contrasts(x.ord) <- contr.treatment(4, base = 4)
x.dummy <- model.matrix( ~ x.ord)[,-1]
x.data <- as.data.frame(x.dummy)

## fit linear regression to get multiple r-squared
x.fit <- lm(x.ord1 ~ x.ord2 + x.ord3, data = x.data)

## extract parameters
bern.prob <- mean(x.data$x.ord1)
r.squared.pred <- summary(x.fit)$adj.r.squared

## change parameters associated with predictor X
pred.dist <- list(dist = "bernoulli", prob = bern.prob)
power.z.logistic(base.prob = 0.15, odds.ratio = 1.416667,
               alpha = 0.05, power = 0.80,
               r.squared.pred = r.squared.pred,
               dist = pred.dist)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Logistic Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Bernoulli
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Odds Ratio (OR)  = 1
#>   H1 (Alternative) : Odds Ratio (OR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 3549  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
