# Factorial Contrasts

Helper function to construct the default contrast coefficients for
various coding schemes.

Note that R has a partial matching feature which allows you to specify
shortened versions of arguments, such as `coding` instead of
`coding.scheme`.

Validated using [`lm()`](https://rdrr.io/r/stats/lm.html) and
[`aov()`](https://rdrr.io/r/stats/aov.html) functions.

## Usage

``` r
factorial.contrasts(factor.levels = c(3, 2),
                    coding.scheme = rep("deviation", length(factor.levels)),
                    base = factor.levels,
                    intercept = FALSE,
                    verbose = TRUE)
```

## Arguments

- factor.levels:

  Integer. Number of levels or groups in each factor. For example, for
  two factors each having two levels or groups use e.g. c(2, 2), for
  three factors each having two levels or groups use e.g. c(2, 2, 2)

- coding.scheme:

  Character vector. Coding scheme for each factor. "sum" for deviation
  or effect coding, "treatment" for dummy coding, "helmert" for Helmert
  type of coding, and "poly" for polynomial coding. Each factor can have
  their own coding scheme. If a single character value is provided, it
  will be copied to other factors

- base:

  Integer vector. Specifies which group is considered the baseline
  group. Ignored for coding schemes other than "treatment"

- intercept:

  Logical. `FALSE` by default. If `TRUE` contrast matrix includes the
  intercept

- verbose:

  Logical. `TRUE` by default. If `FALSE` no output is printed on the
  console

## Value

- factor.levels:

  Number of levels (or groups) in each factor

- factor.data:

  Unique combination of factor levels

- model.matrix:

  Model (design) matrix based on unique combination of factor levels

- contrast.matrix:

  Contrast matrix

## Examples

``` r
###################################################
############### dummy coding scheme ###############
####################################################

# one factor w/ 3 levels
contrast.object <- factorial.contrasts(factor.levels = 3,
                                       coding = "treatment")
#>    A1 A2 A3
#> A1  1  0 -1
#> A2  0  1 -1
# model matrix
contrast.object$model.matrix
#>   (Intercept) A1 A2
#> 1           1  1  0
#> 2           1  0  1
#> 3           1  0  0
#> attr(,"assign")
#> [1] 0 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$A
#>   1 2
#> 1 1 0
#> 2 0 1
#> 3 0 0
#> 

# contrast matrix
contrast.object$contrast.matrix
#>    A1 A2 A3
#> A1  1  0 -1
#> A2  0  1 -1

#######################################################
###############  deviation coding scheme ##############
#######################################################

# especially useful for factorial designs
# two factors w/ 2 and 3 levels, respectively
contrast.object <- factorial.contrasts(factor.levels = c(2, 3),
                                       coding = "sum")
#> Assuming the same coding scheme applies to the other factor(s)
#> Elements of `mu.vector`, `sd.vector`, `n.vector` or `p.vector` should follow this specific order:
#> A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3  
#>        A1:B1  A1:B2  A1:B3  A2:B1  A2:B2  A2:B3
#> A1     0.167  0.167  0.167 -0.167 -0.167 -0.167
#> B1     0.333 -0.167 -0.167  0.333 -0.167 -0.167
#> B2    -0.167  0.333 -0.167 -0.167  0.333 -0.167
#> A1:B1  0.333 -0.167 -0.167 -0.333  0.167  0.167
#> A1:B2 -0.167  0.333 -0.167  0.167 -0.333  0.167

# model matrix
contrast.object$model.matrix
#>   (Intercept) A1 B1 B2 A1:B1 A1:B2
#> 1           1  1  1  0     1     0
#> 2           1  1  0  1     0     1
#> 3           1  1 -1 -1    -1    -1
#> 4           1 -1  1  0    -1     0
#> 5           1 -1  0  1     0    -1
#> 6           1 -1 -1 -1     1     1
#> attr(,"assign")
#> [1] 0 1 2 2 3 3
#> attr(,"contrasts")
#> attr(,"contrasts")$A
#>   [,1]
#> 1    1
#> 2   -1
#> 
#> attr(,"contrasts")$B
#>   [,1] [,2]
#> 1    1    0
#> 2    0    1
#> 3   -1   -1
#> 

# contrast matrix
contrast.object$contrast.matrix
#>            A1:B1      A1:B2      A1:B3      A2:B1      A2:B2      A2:B3
#> A1     0.1666667  0.1666667  0.1666667 -0.1666667 -0.1666667 -0.1666667
#> B1     0.3333333 -0.1666667 -0.1666667  0.3333333 -0.1666667 -0.1666667
#> B2    -0.1666667  0.3333333 -0.1666667 -0.1666667  0.3333333 -0.1666667
#> A1:B1  0.3333333 -0.1666667 -0.1666667 -0.3333333  0.1666667  0.1666667
#> A1:B2 -0.1666667  0.3333333 -0.1666667  0.1666667 -0.3333333  0.1666667


######################################################
###############  Helmert coding scheme ###############
######################################################

# one factor w/ 3 levels
contrast.object <- factorial.contrasts(factor.levels = 3,
                                       coding = "helmert")
#>        A1     A2    A3
#> A1 -0.500  0.500 0.000
#> A2 -0.167 -0.167 0.333

# model matrix
contrast.object$model.matrix
#>   (Intercept) A1 A2
#> 1           1 -1 -1
#> 2           1  1 -1
#> 3           1  0  2
#> attr(,"assign")
#> [1] 0 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$A
#>   [,1] [,2]
#> 1   -1   -1
#> 2    1   -1
#> 3    0    2
#> 

# contrast matrix
contrast.object$contrast.matrix
#>            A1         A2        A3
#> A1 -0.5000000  0.5000000 0.0000000
#> A2 -0.1666667 -0.1666667 0.3333333

#########################################################
###############  polynomial coding scheme ###############
#########################################################

# one factor w/ 3 levels
contrast.object <- factorial.contrasts(factor.levels = 3,
                                       coding = "poly")
#>         A1     A2    A3
#> A.L -0.707  0.000 0.707
#> A.Q  0.408 -0.816 0.408

# model matrix
contrast.object$model.matrix
#>   (Intercept)           A.L        A.Q
#> 1           1 -7.071068e-01  0.4082483
#> 2           1 -7.850462e-17 -0.8164966
#> 3           1  7.071068e-01  0.4082483
#> attr(,"assign")
#> [1] 0 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$A
#>              .L         .Q
#> 1 -7.071068e-01  0.4082483
#> 2 -7.850462e-17 -0.8164966
#> 3  7.071068e-01  0.4082483
#> 

# contrast matrix
contrast.object$contrast.matrix
#>             A1         A2        A3
#> A.L -0.7071068  0.0000000 0.7071068
#> A.Q  0.4082483 -0.8164966 0.4082483
```
