# Element In Set Operator

This operator allows use of set notation style definitions

## Usage

``` r
e1 %e% e2
```

## Arguments

- e1:

  The values to be operated on, on the left hand side

- e2:

  One non-empty character string containing set notation style ranges on
  the real number line. Separate sets with the “&” or “\|” operator for
  AND or OR. Connectors must appear between complete sets, not at the
  start or end of the string, and cannot be doubled.

## Value

a logical vector

## Examples

``` r

c(-1, 0, 1, 9, 10, 16, 17, 20) %e% "(-Inf, 0) | [1, 9) | [10, 16] | (17, Inf]"
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
table(mtcars$mpg %e% "(0, 15.5) | [22.8, 40)")
#> 
#> FALSE  TRUE 
#>    15    17 
table(mtcars$mpg %e% "(0, 15) | [16, 18] | [30, 50)")
#> 
#> FALSE  TRUE 
#>    20    12 
c(-1, 0, 1) %e% "(-Inf, Inf) & [0, 0] | [1, 1]"
#> [1] FALSE  TRUE  TRUE

z <- max(mtcars$mpg)
table(mtcars$mpg %e% "(-Inf, z)")
#> 
#> FALSE  TRUE 
#>     1    31 

## clean up
rm(z)
```
