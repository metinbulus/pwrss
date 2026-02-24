# Power Analysis for Non-parametric Rank-Based Tests (One-Sample, Independent, and Paired Designs)

Calculates power or sample size (only one can be NULL at a time) for
non-parametric rank-based tests. The following tests and designs are
available:

- Wilcoxon Signed-Rank Test (One Sample)

- Wilcoxon Rank-Sum or Mann-Whitney U Test (Independent Samples)

- Wilcoxon Matched-Pairs Signed-Rank Test (Paired Samples)

Formulas are validated using G*Power and tables in the PASS
documentation. However, we adopt the rounding convention used by
G*Power.

## Usage

``` r
power.np.wilcoxon(
  d,
  null.d = 0,
  margin = 0,
  n.ratio = 1,
  n2 = NULL,
  power = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  design = c("independent", "paired", "one.sample"),
  distribution = c("normal", "uniform", "double.exponential", "laplace", "logistic"),
  method = c("guenther", "noether"),
  ceiling = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- d:

  Cohen's d or Hedges' g.

- null.d:

  Cohen's d or Hedges' g under null, typically 0 (zero).

- margin:

  margin - ignorable `d` - `null.d` difference.

- n.ratio:

  `n1 / n2` ratio (applies to independent samples only)

- n2:

  integer; sample size in the second group (or for the single group in
  paired samples or one-sample)

- power:

  statistical power, defined as the probability of correctly rejecting a
  false null hypothesis, denoted as \\1 - \beta\\.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided".

- design:

  character; "independent" (default), "one.sample", or "paired".

- distribution:

  character; parent distribution: "normal", "uniform",
  "double.exponential", "laplace", or "logistic".

- method:

  character; non-parametric approach: "guenther" (default) or "noether"

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

  type of the statistical test (Z- or T-Test).

- df:

  degrees of freedom (applies when method = 'guenther').

- ncp:

  non-centrality parameter for the alternative (applies when method =
  'guenther').

- null.ncp:

  non-centrality parameter for the null (applies when method =
  'guenther').

- t.alpha:

  critical value(s) (applies when method = 'guenther').

- mean:

  mean of the alternative (applies when method = 'noether').

- null.mean:

  mean of the null (applies when method = 'noether').

- sd:

  standard deviation of the alternative (applies when method =
  'noether').

- null.sd:

  standard deviation of the null (applies when method = 'noether').

- z.alpha:

  critical value(s) (applies when method = 'noether').

- power:

  statistical power \\(1-\beta)\\.

- n:

  sample size (`n` or `c(n1, n2)` depending on the design.

## Details

- Use
  [`means.to.d()`](https://metinbulus.github.io/pwrss/reference/means.to.d.md)
  to convert raw means and standard deviations to Cohen's d, and
  [`d.to.cles()`](https://metinbulus.github.io/pwrss/reference/d.to.cles.md)
  to convert Cohen's d to the probability of superiority. Note that this
  interpretation is appropriate only when the underlying distribution is
  approximately normal and the two groups have similar population
  variances.

- NB: `pwrss.np.2means()` function is depreciated and no longer
  supported. `pwrss.np.2groups()` will remain available for some time.

- Note that R has a partial matching feature which allows you to specify
  shortened versions of arguments, such as `alt` instead of
  `alternative`, or `dist` instead of `distribution`.

## References

Al-Sunduqchi, M. S. (1990). *Determining the appropriate sample size for
inferences based on the Wilcoxon statistics* \[Unpublished doctoral
dissertation\]. University of Wyoming - Laramie

Chow, S. C., Shao, J., Wang, H., and Lokhnygina, Y. (2018). *Sample size
calculations in clinical research* (3rd ed.). Taylor & Francis/CRC.

Lehmann, E. (1975). *Nonparameterics: Statistical methods based on
ranks*. McGraw-Hill.

Noether, G. E. (1987). Sample size determination for some common
nonparametric tests. *Journal of the American Statistical Association,
82*(1), 645-647.

Ruscio, J. (2008). A probability-based measure of effect size:
Robustness to base rates and other factors. *Psychological Methods,
13*(1), 19-30.

Ruscio, J., & Mullen, T. (2012). Confidence intervals for the
probability of superiority effect size measure and the area under a
receiver operating characteristic curve. *Multivariate Behavioral
Research, 47*(2), 201-223.

Zhao, Y.D., Rahardja, D., & Qu, Y. (2008). Sample size calculation for
the Wilcoxon-Mann-Whitney test adjusting for ties. *Statistics in
Medicine, 27*(3), 462-468.

## Examples

``` r
# see `?pwrss::power.t.student` for further examples

# Mann-Whitney U or Wilcoxon rank-sum test
# (a.k.a Wilcoxon-Mann-Whitney test) for independent samples

## difference between group 1 and group 2 is not equal to zero
## estimated difference is Cohen'd = 0.25
power.np.wilcoxon(d = 0.25,
                power = 0.80)
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
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
#>   Sample Size          = 265 and 265  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## difference between group 1 and group 2 is greater than zero
## estimated difference is Cohen'd = 0.25
power.np.wilcoxon(d = 0.25,
                power = 0.80,
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= 0
#>   H1 (Alternative) : d - null.d  > 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 208 and 208  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## mean of group 1 is practically not smaller than mean of group 2
## estimated difference is Cohen'd = 0.10 and can be as small as -0.05
power.np.wilcoxon(d = 0.10,
                margin = -0.05,
                power = 0.80,
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 576 and 576  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## mean of group 1 is practically greater than mean of group 2
## estimated difference is Cohen'd = 0.10 and can be as small as 0.05
power.np.wilcoxon(d = 0.10,
                margin = 0.05,
                power = 0.80,
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d <= margin
#>   H1 (Alternative) : d - null.d  > margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 5184 and 5184  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## mean of group 1 is practically same as mean of group 2
## estimated difference is Cohen'd = 0
## and can be as small as -0.05 and as high as 0.05
power.np.wilcoxon(d = 0,
                margin = c(-0.05, 0.05),
                power = 0.80,
                alternative = "two.one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Rank-Sum Test (Independent Samples) 
#> (Wilcoxon-Mann-Whitney or Mann-Whitney U Test)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= min(margin) and
#>                      d - null.d <= max(margin)
#>   H1 (Alternative) : d - null.d  < min(margin) or
#>                      d - null.d  > max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 7175 and 7175  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 


# Wilcoxon signed-rank test for matched pairs (dependent samples)

## difference between time 1 and time 2 is not equal to zero
## estimated difference between time 1 and time 2 is Cohen'd = -0.25
power.np.wilcoxon(d = -0.25,
                power = 0.80,
                design = "paired")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
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
#>   Sample Size          = 134  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## difference between time 1 and time 2 is greater than zero
## estimated difference between time 1 and time 2 is Cohen'd = -0.25
power.np.wilcoxon(d = -0.25,
                power = 0.80,
                design = "paired",
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= 0
#>   H1 (Alternative) : d - null.d  < 0
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 106  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.197
#>   Statistical Power    = 0.803
#> 

## mean of time 1 is practically not smaller than mean of time 2
## estimated difference is Cohen'd = -0.10 and can be as small as 0.05
power.np.wilcoxon(d = -0.10,
                margin = 0.05,
                power = 0.80,
                design = "paired",
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= margin
#>   H1 (Alternative) : d - null.d  < margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 289  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.199
#>   Statistical Power    = 0.801
#> 

## mean of time 1 is practically greater than mean of time 2
## estimated difference is Cohen'd = -0.10 and can be as small as -0.05
power.np.wilcoxon(d = -0.10,
                margin = -0.05,
                power = 0.80,
                design = "paired",
                alternative = "one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= margin
#>   H1 (Alternative) : d - null.d  < margin
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 2599  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 

## mean of time 1 is practically same as mean of time 2
## estimated difference is Cohen'd = 0
## and can be as small as -0.05 and as high as 0.05
power.np.wilcoxon(d = 0,
                margin = c(-0.05, 0.05),
                power = 0.80,
                design = "paired",
                alternative = "two.one.sided")
#> +--------------------------------------------------+
#> |             SAMPLE SIZE CALCULATION              |
#> +--------------------------------------------------+
#> 
#> Wilcoxon Signed-Rank Test (Paired Samples)
#> 
#>   Method       : Guenther
#>   Distribution : Normal
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : d - null.d >= min(margin) and
#>                      d - null.d <= max(margin)
#>   H1 (Alternative) : d - null.d  < min(margin) or
#>                      d - null.d  > max(margin)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Sample Size          = 3589  <<
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.200
#>   Statistical Power    = 0.800
#> 
```
