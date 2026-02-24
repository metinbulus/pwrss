# Power Analysis for Independent Correlations

Calculates power or sample size (only one can be NULL at a time) to test
difference between two independent (Pearson) correlations using Fisher's
z transformation.

Formulas are validated using PASS and G\*Power.

## Usage

``` r
power.z.twocors(
  rho1,
  rho2,
  n2 = NULL,
  n.ratio = 1,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided"),
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- rho1:

  correlation in the first group.

- rho2:

  correlation in the second group.

- n2:

  sample size in the second group. Sample size in the first group can be
  calculated as `n2*kappa`. By default, `n1 = n2` because `n.ratio = 1`.

- n.ratio:

  `n1 / n2` ratio.

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

  statistical power \\(1-\beta)\\

- n:

  sample size for the first and second groups, in the form of c(n1, n2).

## References

Bulus, M., & Polat, C. (2023). pwrss R paketi ile istatistiksel guc
analizi \[Statistical power analysis with pwrss R package\]. *Ahi Evran
Universitesi Kirsehir Egitim Fakultesi Dergisi, 24*(3), 2207-2328.
https://doi.org/10.29299/kefad.1209913

Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2018). Sample size
calculations in clinical research (3rd ed.). Taylor & Francis / CRC.

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
# difference between r1 and r2 is different from zero
# it could be -0.10 as well as 0.10
power.z.twocors(rho1 = 0.20, rho2 = 0.30,
               power = 0.80, alpha = 0.05,
               alternative = "two.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Independent Correlations
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : rho1 - rho2  = 0
#>   H1 (Alternative) : rho1 - rho2 != 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 1380 and 1380  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

# difference between r1 and r2 is greater than zero
power.z.twocors(rho1 = 0.30, rho2 = 0.20,
               power = 0.80, alpha = 0.05,
               alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Independent Correlations
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : rho1 - rho2 <= 0
#>   H1 (Alternative) : rho1 - rho2  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 1088 and 1088  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
