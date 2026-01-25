# Power and Sample Size for Chi-square Goodness-of-Fit or Independence Tests

Calculates power or sample size (only one can be NULL at a time) for
Chi-square goodness-of-fit or independence tests.

NOTE: The `pwrss.chisq.gofit()` function is deprecated. However, it will
remain available as a wrapper for the `power.chisq.gof()` function.

## Usage

``` r
power.chisq.gof(w, null.w = 0, df,
                n = NULL, power = NULL, alpha = 0.05,
                ceiling = TRUE, verbose = TRUE, pretty = FALSE)
```

## Arguments

- w:

  Cohen's w effect size under alternative. It can be any of Cohen's W,
  Phi coefficient, or Cramer's V but degrees of freedom should be
  specified accordingly. Phi coefficient is defined as `sqrt(X2/n)` and
  Cramer's V is defined as `sqrt(X2/(n*v))` where `v` is
  `min(nrow - 1, ncol - 1)` and X2 is the chi-square statistic.

- null.w:

  Cohen's w effect size under null.

- df:

  integer; degrees of freedom. Defined as (n.cells - 1) if `p1` is a
  vector, and as (n.rows - 1) \* (n.cols - 1) if `p1` is a matrix.

- n:

  integer; total sample size.

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

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

  type of the statistical test (Chi-square Test).

- df:

  degrees of freedom.

- ncp:

  non-centrality parameter under alternative.

- null.ncp:

  non-centrality parameter under null.

- chisq.alpha:

  critical value.

- power:

  statistical power \\(1-\beta)\\.

- n:

  total sample size.

## References

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
# ---------------------------------------------------------#
# Example 1: Cohen's W                                     #
# goodness-of-fit test for 1 x k or k x 1 table            #
# How many subjects are needed to claim that               #
# girls choose STEM related majors less than males?       #
# ---------------------------------------------------------#

## Option 1: Use cell probabilities
## from https://www.aauw.org/resources/research/the-stem-gap/
## 28 percent of the  workforce in STEM field is women
prob.vector <- c(0.28, 0.72)
null.prob.vector <- c(0.50, 0.50)
probs.to.w(prob.vector, null.prob.vector)
#>    w   df 
#> 0.44 1.00 

power.chisq.gof(w = 0.44, df = 1,
                alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Chi-Square Test for Goodness-of-Fit or Independence
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : P[i,j] = P0[i,j] for all (i,j) 
#>   H1 (Alt. Claim)   : P[i,j] != P0[i,j] for some (i,j)
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Total Sample Size      = 41  << 
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.196
#>   Statistical Power      = 0.804
#> 


# ---------------------------------------------------------#
# Example 2: Phi Coefficient (or Cramer's V or Cohen's W)  #
# test of independence for 2 x 2 contingency tables        #
# How many subjects are needed to claim that               #
# girls are underdiagnosed with ADHD?                      #
# ---------------------------------------------------------#

## from https://time.com/growing-up-with-adhd/
## 5.6 percent of girls and 13.2 percent of boys are diagnosed with ADHD
prob.matrix <- rbind(c(0.056, 0.132),
                     c(0.944, 0.868))
colnames(prob.matrix) <- c("Girl", "Boy")
rownames(prob.matrix) <- c("ADHD", "No ADHD")
prob.matrix
#>          Girl   Boy
#> ADHD    0.056 0.132
#> No ADHD 0.944 0.868

probs.to.w(prob.matrix)
#>         w        df 
#> 0.1302134 1.0000000 

power.chisq.gof(w = 0.1302134, df = 1,
                alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Chi-Square Test for Goodness-of-Fit or Independence
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : P[i,j] = P0[i,j] for all (i,j) 
#>   H1 (Alt. Claim)   : P[i,j] != P0[i,j] for some (i,j)
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Total Sample Size      = 463  << 
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.200
#>   Statistical Power      = 0.8
#> 


# --------------------------------------------------------#
# Example 3: Cramer's V (or Cohen's W)                    #
# test of independence for j x k contingency tables       #
# How many subjects are needed to detect the relationship #
# between depression severity and gender?                 #
# --------------------------------------------------------#

## from https://doi.org/10.1016/j.jad.2019.11.121
prob.matrix <- cbind(c(0.6759, 0.1559, 0.1281, 0.0323, 0.0078),
                     c(0.6771, 0.1519, 0.1368, 0.0241, 0.0101))
rownames(prob.matrix) <- c("Normal", "Mild", "Moderate",
                           "Severe", "Extremely Severe")
colnames(prob.matrix) <- c("Female", "Male")
prob.matrix
#>                  Female   Male
#> Normal           0.6759 0.6771
#> Mild             0.1559 0.1519
#> Moderate         0.1281 0.1368
#> Severe           0.0323 0.0241
#> Extremely Severe 0.0078 0.0101

probs.to.w(prob.matrix)
#>          w         df 
#> 0.03022008 4.00000000 

power.chisq.gof(w = 0.03022008, df = 4,
                alpha = 0.05, power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Chi-Square Test for Goodness-of-Fit or Independence
#> 
#> ---------------------------------------------------
#> Hypotheses
#> ---------------------------------------------------
#>   H0 (Null Claim)   : P[i,j] = P0[i,j] for all (i,j) 
#>   H1 (Alt. Claim)   : P[i,j] != P0[i,j] for some (i,j)
#> 
#> ---------------------------------------------------
#> Results
#> ---------------------------------------------------
#>   Total Sample Size      = 13069  << 
#>   Type 1 Error (alpha)   = 0.050
#>   Type 2 Error (beta)    = 0.200
#>   Statistical Power      = 0.8
#> 
```
