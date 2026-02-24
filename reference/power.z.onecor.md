# Power Analysis for One-Sample Correlation

Calculates power or sample size (only one can be NULL at a time) to test
a (Pearson) correlation against a constant using Fisher's z
transformation.

Formulas are validated using PASS and G\*Power.

## Usage

``` r
power.z.onecor(
  rho,
  null.rho = 0,
  n = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- rho:

  correlation.

- null.rho:

  correlation when null is true.

- n:

  sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided"
  or "one.sided".

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

  type of the statistical test (Z-Test)

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

  statistical power\\(1-\beta)\\.

- n:

  sample size.

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2018). Sample size
calculations in clinical research (3rd ed.). Taylor & Francis/CRC.

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
# expected correlation is 0.20 and it is different from 0
# it could be 0.20 as well as -0.20
power.z.onecor(rho = 0.20,
               power = 0.80,
               alpha = 0.05,
               alternative = "two.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-Sample Correlation
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : rho - null.rho  = 0
#>   H1 (Alternative) : rho - null.rho != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 194  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# expected correlation is 0.20 and it is greater than 0.10
power.z.onecor(rho = 0.20, null.rho = 0.10,
               power = 0.80,
               alpha = 0.05,
               alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> One-Sample Correlation
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : rho - null.rho <= 0
#>   H1 (Alternative) : rho - null.rho  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 593  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

```
