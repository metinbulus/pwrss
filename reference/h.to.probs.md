# Conversion from Probability Difference to Cohen's h

Helper function to convert probability difference to Cohen's h (and vice
versa).

## Usage

``` r
h.to.probs(h, prob1 = NULL, prob2 = NULL, verbose = 1)
```

## Arguments

- h:

  Cohen's h effect size.

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

## Value

- h:

  Cohen's h effect size.

- prob1:

  probability of success in the first group, or under the alternative
  hypothesis in the one-sample case).

- prob2:

  probability of success in the second group, or under the null
  hypothesis in the one-sample case).

## Details

h and either prob1 or prob2 are required

## Examples

``` r
h.to.probs(h = 0.1, prob1 = 0.56)
#>     prob1     prob2         h 
#> 0.5600000 0.5101442 0.1000000 
h.to.probs(h = 0.1, prob2 = 0.50)
#>     prob1     prob2         h 
#> 0.5499167 0.5000000 0.1000000 
```
