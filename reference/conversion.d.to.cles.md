# Conversion from Cohen's d to Common Language Effect Size

Helper function to convert Cohen's d to common language effect size (or
vice versa). The result is the probability of superiority for
independent samples. It can be interpreted as the probability that a
randomly selected observation from Group 1 exceeds a randomly selected
observation from Group 2. The rationale is the same for paired-samples
and one-sample designs, but the interpretation differs: For paired
samples, it can be interpreted as the probability that the difference
score (i.e., the score under Condition 1 minus the score under Condition
2) is greater than zero for a randomly selected individual. For a
one-sample design, it can be interpreted as the probability that a
randomly selected observation is greater than the reference value (e.g.,
0).

## Usage

``` r
d.to.cles(d, design = c("independent", "paired", "one.sample"), verbose = TRUE)

  cles.to.d(cles, design = c("independent", "paired", "one.sample"), verbose = TRUE)
```

## Arguments

- d:

  Cohen's d

- design:

  character; one of the "independent", "paired", or "one.sample". The
  default is "independent".

- cles:

  common language effect size.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

## Value

- d:

  Cohen's d

- cles:

  common language effect size.

## Examples

``` r
d.to.cles(0.20) # small
#>      cles         d 
#> 0.5562315 0.2000000 
d.to.cles(0.50) # medium
#>      cles         d 
#> 0.6381632 0.5000000 
d.to.cles(0.80) # large
#>      cles         d 
#> 0.7141962 0.8000000 

cles.to.d(0.5562315)
#>         d      cles 
#> 0.2000002 0.5562315 
cles.to.d(0.6381632)
#>         d      cles 
#> 0.5000000 0.6381632 
cles.to.d(0.7141962)
#>         d      cles 
#> 0.8000001 0.7141962 
```
