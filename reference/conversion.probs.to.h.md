# Conversion from Probability Difference to Cohen's h

Helper function to convert probability difference to Cohen's h (and vice
versa).

## Usage

``` r
probs.to.h(prob1, prob2 = 0.50, verbose = TRUE)
```

## Arguments

- prob1:

  probability of success in the first group, or under the alternative
  hypothesis in the one-sample case).

- prob2:

  probability of success in the second group, or under the null
  hypothesis in the one-sample case).

- h:

  Cohen's h effect size.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

## Value

- p1:

  probability of success in the first group, or under the alternative
  hypothesis in the one-sample case).

- p2:

  probability of success in the second group, or under the null
  hypothesis in the one-sample case).

- h:

  Cohen's h effect size.

## Examples

``` r
probs.to.h(prob1 = 0.56, prob2 = 0.50)
#>         h     prob1     prob2 
#> 0.1202899 0.5600000 0.5000000 
```
