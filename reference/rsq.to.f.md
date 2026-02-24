# Conversion from R-squared to Cohen's f

Helper function to convert between Cohen's f and R-squared.

## Usage

``` r
rsq.to.f(r.squared.full, r.squared.reduced = 0, verbose = 0)
```

## Arguments

- r.squared.full:

  R-squared for the full model.

- r.squared.reduced:

  R-squared for the reduced model.

- verbose:

  `1` by default (returns results), if `0` no output is printed on the
  console.

## Value

- f.squared:

  Cohen's f-squared.

- f:

  Cohen's f.

- r.squared.full:

  R-squared for the full model.

- r.squared.reduced:

  R-squared for the reduced model.

## References

Cohen, J. (1988). Statistical power analysis for the behavioral sciences
(2nd ed.). Lawrence Erlbaum Associates.

Selya, A. S., Rose, J. S., Dierker, L. C., Hedeker, D., & Mermelstein,
R. J. (2012). A practical guide to calculating Cohen's f2, a measure of
local effect size, from PROC MIXED. *Frontiers in Psychology, 3*,
Article 111. https://doi.org/10.3389/fpsyg.2012.00111

## Examples

``` r
  rsq.to.f(r.squared.full = 0.02) # small
  rsq.to.f(r.squared.full = 0.13) # medium
  rsq.to.f(r.squared.full = 0.26) # large
```
