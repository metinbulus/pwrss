# Conversion from Cohen's f to R-squared

Helper function to convert between Cohen's f and R-squared.

## Usage

``` r
rsq.to.f(r.squared.full, r.squared.reduced = 0, verbose = TRUE)

  f.to.rsq(f, r.squared.full = NULL, verbose = TRUE)
```

## Arguments

- f:

  Cohen's f.

- r.squared.full:

  R-squared for the full model.

- r.squared.reduced:

  R-squared for the reduced model.

- verbose:

  logical; whether the output should be printed on the console. `TRUE`
  by default.

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
local effect size, from PROC MIXED. Frontiers in Psychology, 3, 111.
[doi:10.3389/fpsyg.2012.00111](https://doi.org/10.3389/fpsyg.2012.00111)

## Examples

``` r
  f.to.rsq(f = 0.10) # small
#>         f.squared                 f    r.squared.full r.squared.reduced 
#>        0.01000000        0.10000000        0.00990099        0.00000000 
  f.to.rsq(f = 0.25) # medium
#>         f.squared                 f    r.squared.full r.squared.reduced 
#>        0.06250000        0.25000000        0.05882353        0.00000000 
  f.to.rsq(f = 0.40) # large
#>         f.squared                 f    r.squared.full r.squared.reduced 
#>          0.160000          0.400000          0.137931          0.000000 
```
