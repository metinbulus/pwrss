# Conversion from Probability Difference to Cohen's h

Helper function to convert probability difference to Cohen's h (and vice
versa).

## Usage

``` r
probs.to.h(prob1, prob2 = 0.5, verbose = 1)
```

## Arguments

- prob1:

  Probability of success in the first group, or under the alternative
  hypothesis in the one-sample case).

- prob2:

  Probability of success in the second group, or under the null
  hypothesis in the one-sample case).

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

- h:

  Cohen's h effect size.

## Value

- h:

  Cohen's h effect size.

- prob1:

  probability of success in the first group, or under the alternative
  hypothesis in the one-sample case).

- prob2:

  probability of success in the second group, or under the null
  hypothesis in the one-sample case).

## Examples

``` r
probs.to.h(prob1 = 0.56, prob2 = 0.50)
#>         h     prob1     prob2 
#> 0.1202899 0.5600000 0.5000000 
```
