# Conversion from z to probability

Helper function to convert z scores into a probability.

## Usage

``` r
z.to.prob(z, null.prob, n, alternative, arcsine, correct, std.error)
```

## Arguments

- z:

  standard score

- null.prob:

  probability of success under null.

- n:

  integer; sample size.

- alternative:

  character; the direction or type of the hypothesis test: "two.sided",
  "one.sided", or "two.one.sided".

- arcsine:

  logical; whether arcsine transformation should be applied. `FALSE` by
  default. Note that when `arcsine = TRUE`, any specification to
  `correct` and `std.error` will be ignored.

- correct:

  logical; whether Yate's continuity correction should be applied.

- std.error:

  character; whether to calculate standard error using "null" or
  "alternative" value. "null" by default.

## Value

- prob:

  probability of success under alternative.

- null.sd:

  standard deviation of the null distribution.

## Examples

``` r
# TBA...
```
