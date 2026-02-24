# Conversion from Eta-squared to Cohen's f

Conversion from Eta-squared to Cohen's f

## Usage

``` r
etasq.to.f(eta.squared, verbose = 1)
```

## Arguments

- eta.squared:

  (Partial) Eta-squared.

- verbose:

  `1` by default (returns test, hypotheses, and results), if `2` a more
  detailed output is given (plus key parameters and definitions), if `0`
  no output is printed on the console.

## Value

- f:

  Cohen's f.

- f.squared:

  Cohen's f².

- eta.squared:

  (Partial) Eta-squared.

## References

Cohen, J. (1988). *Statistical power analysis for the behavioral
sciences* (2nd ed.). Lawrence Erlbaum Associates.

## Examples

``` r
etasq.to.f(eta.squared = 0.01) # small
#>   f.squared           f eta.squared 
#>  0.01010101  0.10050378  0.01000000 
etasq.to.f(eta.squared = 0.06) # medium
#>   f.squared           f eta.squared 
#>  0.06382979  0.25264558  0.06000000 
etasq.to.f(eta.squared = 0.14) # large
#>   f.squared           f eta.squared 
#>   0.1627907   0.4034733   0.1400000 
```
