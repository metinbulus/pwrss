# Conversion from Means and Standard Deviations to Cohen's d

Helper function to convert means and standard deviations to Cohen's d.

## Usage

``` r
means.to.d(
  mu1,
  mu2 = 0,
  sd1 = 1,
  sd2 = 1,
  n.ratio = 1,
  n2,
  paired = FALSE,
  rho.paired = 0.5,
  verbose = 1
)
```

## Arguments

- mu1:

  mean of the first group.

- mu2:

  mean of the second group.

- sd1:

  standard deviation of the first group.

- sd2:

  standard deviation of the second group.

- n.ratio:

  `n1 / n2` ratio (applies to independent samples only).

- n2:

  integer; sample size in the second group (or for the single group in
  paired samples).

- paired:

  if `TRUE` paired samples

- rho.paired:

  correlation between repeated measures for paired samples (e.g.,
  pretest and post-test).

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

- parms:

  list of parameters used in calculation.

- d:

  Cohen's d

- pooled.sd:

  Pooled standard deviation

- var.ratio:

  Ratio of the variance in the two groups (applies to independent
  samples only)

- n1:

  Sample size group 1 (applies to independent samples only)

- n2:

  Sample size group 2

## Examples

``` r

# means and standard deviations from independent samples
means.to.d(mu1 = 20, mu2 = 17.5,
           sd1 = 5, sd2 = 15,
           n2 = 30, n.ratio = 1)
#> Warning: Interpretation of Cohen's d may no longer be valid when variances differ beyond sampling error.
#>         d 
#> 0.2236068 

# means and standard deviations from paired samples
means.to.d(mu1 = 20, mu2 = 17.5,
           sd1 = 5, sd2 = 15,
           n2 = 30, n.ratio = 1,
           paired = TRUE,
           rho.paired = 0.50)
#> Warning: Interpretation of Cohen's d may no longer be valid when variances differ beyond sampling error.
#>         d 
#> 0.1889822 
```
