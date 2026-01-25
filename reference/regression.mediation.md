# Power Analysis for Indirect Effects in a Mediation Model (Z, Joint, and Monte Carlo Tests)

Calculates power or sample size (only one can be NULL at a time) to test
indirect effects in a mediation model (Z-Test, Joint Test, and Monte
Carlo Interval Test). One can consider explanatory power of the
covariates in the mediator and outcome model via specifying R-squared
values accordingly. `power.z.mediation()` and `power.z.med()` are the
same functions.

NOTE: The function `pwrss.z.mediation()` (or its alias `pwrss.z.med()`)
are no longer supported. However, they will remain available as wrappers
for the `power.z.mediation` function.

Formulas are validated using Monte Carlo simulation.

## Usage

``` r
power.z.mediation(beta.a, beta.b, beta.cp = 0,
                  sd.predictor = 1, sd.mediator = 1, sd.outcome = 1,
                  r.squared.mediator = beta.a^2 * sd.predictor^2 / sd.mediator^2,
                  r.squared.outcome = (beta.b^2 * sd.mediator^2 +
                                         beta.cp^2 * sd.predictor^2) / sd.outcome^2,
                  n = NULL, power = NULL, alpha = 0.05,
                  alternative = c("two.sided", "one.sided"),
                  method = c("sobel", "aroian", "goodman",
                             "joint", "monte.carlo"),
                  n.simulation = 1000,
                  n.draws = 1000,
                  ceiling = TRUE,
                  verbose = TRUE,
                  pretty = FALSE)
```

## Arguments

- beta.a:

  regression coefficient for X -\> M path. One can use standardized
  regression coefficient, but should keep `sd.predictor = 1` and
  `sd.mediator = 1` or leave them out as they are default
  specifications.

- beta.b:

  regression coefficient for M -\> Y path. One can use standardized
  regression coefficient, but should keep `sd.mediator = 1` and
  `sd.outcome = 1` or leave them out as they are default specifications.

- beta.cp:

  regression coefficient for X -\> Y path (the direct path). One can use
  standardized regression coefficient, but should keep
  `sd.predictor = 1` and `sd.outcome = 1` or leave them out as they are
  default specifications.

- sd.predictor:

  standard deviation of the predictor (X). For a binary predictor,
  `sd.predictor = sqrt(p*(1-p))` where`p` is the proportion of subjects
  in one of the groups.

- sd.mediator:

  standard deviation of the mediator (M).

- sd.outcome:

  standard deviation of the outcome (Y).

- r.squared.mediator:

  R-squared value for the mediator model (M ~ X). The default is
  `r.squared.mediator = beta.a^2 * sd.predictor^2 / sd.mediator^2`
  assuming that X is the only predictor. Thus, an `r.squared.mediator`
  below this value will throw a warning. To consider other covariates in
  the mediator model provide a value greater than the default.

- r.squared.outcome:

  R-squared value for the outcome model (Y ~ M + X). The default is
  `r.squared.outcome = (beta.b^2 * sd.mediator^2 + beta.cp^2 * sd.predictor^2) / sd.outcome^2`
  assuming that M and X are the only predictors. Thus, an
  `r.squared.outcome` below this value will throw a warning. To consider
  other covariates in the outcome model provide a value greater than the
  default.

- n:

  integer; sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; direction or type of the hypothesis test: "two.sided" or
  "one.sided".

- method:

  character; "sobel", "aroian", "goodman", "joint" or "monte.carlo".
  "joint" and "monte.carlo" methods cannot be used for sample size
  calculation.

- n.simulation:

  integer; number of replications (applies when method = "monte.carlo").

- n.draws:

  integer; number of draws from the distribution of the path
  coefficients for each replication (applies when method =
  "monte.carlo").

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

  type of the statistical test ("Z-Test", "Joint Test", or "Monte Carlo
  Interval Test").

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

## References

Aroian, L. A. (1947). The probability function of the product of two
normally distributed variables. Annals of Mathematical Statistics,
18(2), 265-271.

Goodman, L. A. (1960). On the exact variance of products. Journal of the
American Statistical Association, 55(292), 708-713.

MacKinnon, D. P., & Dwyer, J. H. (1993). Estimating mediated effects in
prevention studies. Evaluation Review, 17(2), 144-158.

MacKinnon, D. P., Warsi, G., & Dwyer, J. H. (1995). A simulation study
of mediated effect measures. Multivariate Behavioral Research, 30(1),
41-62.

Preacher, K. J., & Hayes, A. F. (2004). SPSS and SAS procedures for
estimating indirect effects in simple mediation models. Behavior
Research Methods, Instruments, & Computers, 36, 717-731.

Preacher, K. J., & Hayes, A. F. (2008). Asymptotic and resampling
strategies for assessing and comparing indirect effects in multiple
mediator models. Behavior Research Methods, 40, 879-891.

Sobel, M. E. (1982). Asymptotic intervals for indirect effects in
structural equations models. In S. Leinhart (Ed.), Sociological
methodology 1982 (pp. 290-312). Jossey-Bass.

## Examples

``` r
# with standardized coefficients

## statistical power
power.z.mediation(beta.a = 0.25,
            beta.b = 0.25,
            beta.cp = 0.10,
            n = 200)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Indirect Effect in a Mediation Model
#> 
#>   Method            : Sobel
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : beta[a*b] = 0 
#>   H1 (Alt. Claim)   : beta[a*b] != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 200
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error         = 0.279
#>   Statistical Power    = 0.721  <<
#> 

## minimum required sample size
power.z.mediation(beta.a = 0.25,
            beta.b = 0.25,
            beta.cp = 0.10,
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Indirect Effect in a Mediation Model
#> 
#>   Method            : Sobel
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : beta[a*b] = 0 
#>   H1 (Alt. Claim)   : beta[a*b] != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 242  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error         = 0.200
#>   Statistical Power    = 0.8
#> 

## adjust for covariates in the outcome model
power.z.mediation(beta.a = 0.25,
            beta.b = 0.25,
            beta.cp = 0.10,
            r.squared.outcome = 0.50,
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Indirect Effect in a Mediation Model
#> 
#>   Method            : Sobel
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : beta[a*b] = 0 
#>   H1 (Alt. Claim)   : beta[a*b] != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 185  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error         = 0.199
#>   Statistical Power    = 0.801
#> 

# with binary predictor X such as treatment/control variable
# in this case standardized coefficients for path a and cp would be Cohen's d values

## statistical power
p <- 0.50 # proportion of subjects in one group
power.z.mediation(beta.a = 0.40,
            beta.b = 0.25,
            beta.cp = 0.10,
            sd.predictor = sqrt(p*(1-p)),
            n = 200)
#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Indirect Effect in a Mediation Model
#> 
#>   Method            : Sobel
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : beta[a*b] = 0 
#>   H1 (Alt. Claim)   : beta[a*b] != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 200
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error         = 0.387
#>   Statistical Power    = 0.613  <<
#> 

## minimum required sample size
power.z.mediation(beta.a = 0.40,
            beta.b = 0.25,
            beta.cp = 0.10,
            sd.predictor = sqrt(p*(1-p)),
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Indirect Effect in a Mediation Model
#> 
#>   Method            : Sobel
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : beta[a*b] = 0 
#>   H1 (Alt. Claim)   : beta[a*b] != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 311  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error         = 0.200
#>   Statistical Power    = 0.8
#> 

## adjust for covariates in the outcome model
power.z.mediation(beta.a = 0.40,
            beta.b = 0.25, beta.cp = 0.10,
            r.squared.outcome = 0.50,
            sd.predictor = sqrt(p*(1-p)),
            power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Indirect Effect in a Mediation Model
#> 
#>   Method            : Sobel
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : beta[a*b] = 0 
#>   H1 (Alt. Claim)   : beta[a*b] != 0 
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Sample Size          = 254  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error         = 0.200
#>   Statistical Power    = 0.8
#> 
```
