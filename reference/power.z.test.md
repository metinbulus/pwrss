# Statistical Power for the Generic z-Test

Calculates power for the generic z-Test with (optional) Type 1 and Type
2 error plots.

## Usage

``` r
power.z.test(
  mean = NULL,
  sd = 1,
  null.mean = 0,
  null.sd = 1,
  alpha = 0.05,
  alternative = c("two.sided", "one.sided", "two.one.sided"),
  plot = TRUE,
  verbose = 1,
  utf = FALSE
)
```

## Arguments

- mean:

  mean of the alternative.

- sd:

  standard deviation of the alternative. Do not change this value except
  when some sort of variance correction is applied (e.g. as in logistic
  and Poisson regressions).

- null.mean:

  mean of the null. When alternative = "two.one.sided", the function
  expects two values in the form `c(lower, upper)`. If a single value is
  provided, it is interpreted as the absolute bound and automatically
  expanded to `c(-value, +value)`.

- null.sd:

  standard deviation of the null. Do not change this value except when
  some sort of correction is applied.

- alpha:

  type 1 error rate, defined as the probability of incorrectly rejecting
  a true null hypothesis, denoted as \\\alpha\\.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided". "two.one.sided" is used for
  equivalence and minimal effect testing.

- plot:

  logical; `FALSE` switches off Type 1 and Type 2 error plot. `TRUE` by
  default.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

- utf:

  logical; whether the output should show Unicode characters (if
  encoding allows for it). `FALSE` by default.

## Value

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

## Examples

``` r
# two-sided
# power defined as the probability of observing test statistics greater than
# the positive critical value OR less than the negative critical value
power.z.test(mean = 1.96, alpha = 0.05, alternative = "two.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic z-Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : mean  = null.mean
#>   H1 (Alternative) : mean != null.mean
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.500
#>   Statistical Power    = 0.500  <<
#> 

# one-sided
# power is defined as the probability of observing a test statistic greater
# than the critical value
power.z.test(mean = 1.96, alpha = 0.05, alternative = "one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic z-Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : mean <= null.mean
#>   H1 (Alternative) : mean  > null.mean
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.376
#>   Statistical Power    = 0.624  <<
#> 

# equivalence
# power is defined as the probability of observing a test statistic greater
# than the upper critical value (for the lower bound) AND less than the
# lower critical value (for the upper bound)
power.z.test(mean = 0, null.mean = c(-2, 2), alpha = 0.05,
             alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic z-Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : mean <= min(null.mean) or
#>                      mean >= max(null.mean)
#>   H1 (Alternative) : mean  > min(null.mean) and
#>                      mean  < max(null.mean)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.722
#>   Statistical Power    = 0.278  <<
#> 

# minimal effect testing
# power is defined as the probability of observing a test statistic greater
# than the upper critical value (for the upper bound) OR less than the lower
# critical value (for the lower bound).
power.z.test(mean = 2, null.mean = c(-1, 1), alpha = 0.05,
             alternative = "two.one.sided")

#> +--------------------------------------------------+
#> |                POWER CALCULATION                 |
#> +--------------------------------------------------+
#> 
#> Generic z-Test
#> 
#> ----------------------------------------------------
#> Hypotheses
#> ----------------------------------------------------
#>   H0 (Null)        : mean >= min(null.mean) and
#>                      mean <= max(null.mean)
#>   H1 (Alternative) : mean  < min(null.mean) or
#>                      mean  > max(null.mean)
#> 
#> ----------------------------------------------------
#> Results
#> ----------------------------------------------------
#>   Type 1 Error (alpha) = 0.050
#>   Type 2 Error (beta)  = 0.831
#>   Statistical Power    = 0.169  <<
#> 
```
