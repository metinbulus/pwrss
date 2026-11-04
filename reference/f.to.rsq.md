# Conversion from Cohen's f to R-squared

Helper function to convert between Cohen's f and R-squared.

## Usage

``` r
f.to.rsq(f, r.squared.full = NULL, verbose = 1)
```

## Arguments

- f:

  Cohen's f.

- r.squared.full:

  R-squared for the full model.

- verbose:

  `1` by default (returns results), if `0` no output is printed on the
  console.

## Value

- f:

  Cohen's f.

- f.squared:

  Cohen's f-squared.

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

  f.to.rsq(f = 0.14) # small
#>         f.squared                 f    r.squared.full r.squared.reduced 
#>        0.01960000        0.14000000        0.01922322        0.00000000 
  f.to.rsq(f = 0.39) # medium
#>         f.squared                 f    r.squared.full r.squared.reduced 
#>         0.1521000         0.3900000         0.1320198         0.0000000 
  f.to.rsq(f = 0.59) # large
#>         f.squared                 f    r.squared.full r.squared.reduced 
#>         0.3481000         0.5900000         0.2582153         0.0000000 
```
