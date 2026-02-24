# Power Analysis for Poisson Regression Coefficient (Wald's z Test)

Calculates power or sample size (only one can be NULL at a time) to test
a single coefficient in poisson regression. `power.z.poisson()` and
`power.z.poisreg()` are the same functions, as well as
`pwrss.z.poisson()` and `pwrss.z.poisreg()`. The distribution of the
predictor variable can be one of the following:
`c("normal", "poisson", "uniform", "exponential", "binomial", "bernouilli", "lognormal")`.
The default parameters for these distributions are

`distribution = list(dist = "normal", mean = 0, sd = 1)`  
`distribution = list(dist = "poisson", lambda = 1)`  
`distribution = list(dist = "uniform", min = 0, max = 1)`  
`distribution = list(dist = "exponential", rate = 1)`  
`distribution = list(dist = "binomial", size = 1, prob = 0.50)`  
`distribution = list(dist = "bernoulli", prob = 0.50)`  
`distribution = list(dist = "lognormal", meanlog = 0, sdlog = 1)`  

Parameters defined in [`list()`](https://rdrr.io/r/base/list.html) form
can be modified, but the names should be kept the same. It is sufficient
to use distribution's name for default parameters (e.g.
`dist = "normal"`).

Formulas are validated using Monte Carlo simulation, G\*Power, and
tables in the PASS documentation.

## Usage

``` r
power.z.poisson(
  base.rate = NULL,
  rate.ratio = NULL,
  beta0 = NULL,
  beta1 = NULL,
  n = NULL,
  power = NULL,
  r.squared.predictor = 0,
  mean.exposure = 1,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
  method = c("demidenko(vc)", "demidenko", "signorini"),
  distribution = "normal",
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- base.rate:

  the base mean event rate.

- rate.ratio:

  event rate ratio. The relative increase in the mean event rate for one
  unit increase in the predictor (similar to odds ratio in logistic
  regression).

- beta0:

  the natural logarithm of the base mean event rate.

- beta1:

  the natural logarithm of the relative increase in the mean event rate
  for one unit increase in the predictor.

- n:

  integer; sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- r.squared.predictor:

  proportion of variance in the predictor accounted for by other
  covariates. This is not a pseudo R-squared. To compute it, regress the
  predictor on the covariates and extract the adjusted R-squared from
  that model.

- mean.exposure:

  the mean exposure time (should be \> 0), usually it is 1.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided"
  or "one.sided".

- method:

  character; calculation method: `"demidenko(vc)"` stands for
  Demidenko (2007) procedure with variance correction; `"demidenko"`
  stands for Demidenko (2007) procedure without variance correction;
  `"signorini"` stands for Signorini (1991) procedure. `"demidenko"` and
  `"signorini"` methods produce similar results but `"demidenko(vc)"` is
  more precise.

- distribution:

  character; distribution family. Can be one of the
  `c("normal", "poisson", "uniform", "exponential", "binomial", "bernouilli", "lognormal")`.

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

  statistical power \\(1-\beta)\\

- n:

  sample size.

## Details

- NB: The `pwrss.z.poisson()` and its alias `pwrss.z.poisreg()` are
  deprecated. However, they will remain available as wrappers for the
  [`power.z.logistic()`](https://metinbulus.github.io/pwrss/reference/power.z.logistic.md)
  function during a transition period.

## References

Demidenko, E. (2007). Sample size determination for logistic regression
revisited. *Statistics in Medicine, 26*(18), 3385-3397.
https://doi.org/10.1002/sim.2771

Signorini, D. F. (1991). Sample size for Poisson regression.
*Biometrika, 78*(2), 446-450.

## Examples

``` r
# predictor X follows normal distribution

## regression coefficient specification
power.z.poisson(beta0 = 0.50, beta1 = -0.10,
                alpha = 0.05, power = 0.80,
                dist = "normal")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Poisson Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Rate Ratio (RR)  = 1
#>   H1 (Alternative) : Rate Ratio (RR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 474  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## rate ratio specification
power.z.poisson(base.rate = exp(0.50),
                rate.ratio = exp(-0.10),
                alpha = 0.05, power = 0.80,
                dist = "normal")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Poisson Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Rate Ratio (RR)  = 1
#>   H1 (Alternative) : Rate Ratio (RR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 474  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## change parameters associated with predictor X
dist.x <- list(dist = "normal", mean = 10, sd = 2)
power.z.poisson(base.rate = exp(0.50),
                rate.ratio = exp(-0.10),
                alpha = 0.05, power = 0.80,
                dist = dist.x)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Poisson Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Rate Ratio (RR)  = 1
#>   H1 (Alternative) : Rate Ratio (RR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 318  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 


# predictor X follows Bernoulli distribution (such as treatment/control groups)

## regression coefficient specification
power.z.poisson(beta0 = 0.50, beta1 = -0.10,
                alpha = 0.05, power = 0.80,
                dist = "bernoulli")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Poisson Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Bernoulli
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Rate Ratio (RR)  = 1
#>   H1 (Alternative) : Rate Ratio (RR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 2003  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## rate ratio specification
power.z.poisson(base.rate = exp(0.50),
                rate.ratio = exp(-0.10),
                alpha = 0.05, power = 0.80,
                dist = "bernoulli")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Poisson Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Bernoulli
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Rate Ratio (RR)  = 1
#>   H1 (Alternative) : Rate Ratio (RR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 2003  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## change parameters associatied with predictor X
dist.x <- list(dist = "bernoulli", prob = 0.30)
power.z.poisson(base.rate = exp(0.50),
                rate.ratio = exp(-0.10),
                alpha = 0.05, power = 0.80,
                dist = dist.x)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Poisson Regression Coefficient (Wald's Z-Test)
#> 
#>   Method          : Demidenko (Variance Corrected)
#>   Predictor Dist. : Bernoulli
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : Rate Ratio (RR)  = 1
#>   H1 (Alternative) : Rate Ratio (RR) != 1
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 2404  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
