# Distribution functions for the Lambda prime / non-central Lambda distribution

Density, distribution function, quantile function and random generation
for the lambda prime distribution.

## Usage

``` r
dlambdap(x, df, ncp, log = FALSE)
plambdap(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
qlambdap(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
rlambdap(n, df, ncp)

qlambdap(p, df, ncp, lower.tail = TRUE, log.p = FALSE)

dlambdap(x, df, ncp, log = FALSE)

rlambdap(n, df, ncp)
```

## Arguments

- df:

  the degrees of freedom of the distribution

- ncp:

  the non-centrality parameter (t) of the distribution

- lower.tail:

  logical; if `TRUE` (default), probabilities are \\P\[X \<= x\]\\,
  otherwise, \\P\[X \> x\]\\

- p:

  vector of probabilities

- x, q:

  vector of quantiles

- log, log.p:

  logical; if `TRUE`, probabilities / densities are given as logarithms

- n:

  number of observations

## Value

`dlambdap` gives the density, `plambdap` gives the distribution function
(probabilities), `qlambdap` gives the quantile function, and `rlambdap`
generates a random vector with lambda prime distributed values.

## Details

These functions compute LeCoutre's Lambda prime \\\Lambda'\\
distribution with df degrees of freedom (denoted df or \\\nu\\) and a
non-centrality parameter (denoted ncp or t, and being the observed
t-statistic). It is a continuous probability distribution that
frequently arises in the sampling distribution of confidence limits for
a normal mean and for inferences regarding signal-to-noise or
standardized effect sizes. The distribution is generally asymmetric, and
its shape adapts based on its parameters. When the non-centrality
parameter (t) is zero, or if the degrees of freedom grow large
(\\\chi^2\_{df} / df \to 0\\), it reduces / converges to the standard
normal distribution. Formally: \$\$\Lambda'\_{df}(t) = z + t
\sqrt{\chi^2\_{df} / df}\$\$ The non-central t distribution is the
non-centrality parameter \\\Lambda\\ plus the standard normal z
distribution, all divided by the square root of the usual chi-square
distribution divided by the degrees of freedom: \$\$t'\_{df}(\Lambda) =
(\Lambda + z) / \sqrt{\chi^2\_{df} / df}\$\$ A \\\Lambda'\\ distributed
random variable can be viewed as a confidence level on a non-central t
(with the confidence intervals being computed as percent points of the
\\\Lambda'\\ distribution).

## References

LeCoutre, B. (2007). Another look at confidence intervals for the
noncentral t distribution. Journal of Modern Applied Statistical
Methods, 6(1), 107–116. https://doi.org/10.22237/jmasm/1177992600

## See also

t distribution functions:
[`stats::dt()`](https://rdrr.io/r/stats/TDist.html),
[`stats::pt()`](https://rdrr.io/r/stats/TDist.html),
[`stats::qt()`](https://rdrr.io/r/stats/TDist.html), and
[`stats::rt()`](https://rdrr.io/r/stats/TDist.html).

## Examples

``` r
set.seed(1)
dlambdap(11.1, df = 9, ncp = 10) # 0.1294471
#> [1] 0.1294471
plambdap(11.1, df = 9, ncp = 10) # 0.7134134
#> [1] 0.7134134
qlambdap(0.01, df = 9, ncp = 10) # 4.245347
#> [1] 4.245347
rv <- rlambdap(100, df = 50, ncp = 2)
mean(rv) # 2.077029
#> [1] 2.077029
pv <- plambdap(rv,  df = 50, ncp = 2)
summary(pv)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.01715 0.31579 0.51642 0.52259 0.74839 0.99703 
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.01715 0.31579 0.51642 0.52259 0.74839 0.99703
qv <- qlambdap(pv,  df = 50, ncp = 2)
summary(qv)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -0.1677  1.5007  2.0319  2.0770  2.6726  4.7966 
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  -0.1677  1.5007  2.0319  2.0770  2.6726  4.7966
# absolute difference between the original random vector and
# the quantile vector calculated from the probabilities of the
# original random vector (< 1e-12)
max(abs(qv - rv)) # 0.0000000000002498002
#> [1] 2.498002e-13
```
