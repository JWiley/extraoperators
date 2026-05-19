# Several ways to evaluate whether any values meet logical conditions

Several ways to evaluate whether any values meet logical conditions

## Usage

``` r
e1 %anygele% e2

e1 %anygel% e2

e1 %anygle% e2

e1 %anygl% e2

e1 %anyge% e2

e1 %anyg% e2

e1 %anyle% e2

e1 %anyl% e2

e1 %anyin% e2

e1 %any!in% e2

e1 %anynin% e2

e1 %any==% e2

e1 %any!=% e2

e1 %anyc% e2

e1 %anye% e2

e1 %anygrepl% e2

e1 %any!grepl% e2

e1 %anyflipIn% e2
```

## Arguments

- e1:

  A vector to be evaluated.

- e2:

  The right hand side value passed to the underlying logical operator.
  See
  [`logicals`](https://joshuawiley.com/extraoperators/reference/logicals.md),
  [`%c%`](https://joshuawiley.com/extraoperators/reference/grapes-c-grapes.md),
  and
  [`%e%`](https://joshuawiley.com/extraoperators/reference/grapes-e-grapes.md)
  for operator-specific requirements.

## Value

A logical value whether any `e1` meet the logical conditions.

## Examples

``` r

1:5 %anygele% c(2, 4)
#> [1] TRUE
1:5 %anygele% c(6, 7)
#> [1] FALSE

1:5 %anygel% c(2, 4)
#> [1] TRUE
1:5 %anygel% c(6, 7)
#> [1] FALSE

1:5 %anygle% c(2, 4)
#> [1] TRUE
1:5 %anygle% c(5, 6)
#> [1] FALSE

1:5 %anygl% c(2, 4)
#> [1] TRUE
1:5 %anygl% c(0, 1)
#> [1] FALSE

1:5 %anyge% 2
#> [1] TRUE
1:5 %anyge% 6
#> [1] FALSE

1:5 %anyg% 2
#> [1] TRUE
1:5 %anyg% 5
#> [1] FALSE

1:5 %anyle% 2
#> [1] TRUE
1:5 %anyle% 0
#> [1] FALSE

1:5 %anyl% 2
#> [1] TRUE
1:5 %anyl% 1
#> [1] FALSE

1:5 %anyin% c(2, 99)
#> [1] TRUE
c("jack", "jill", "john", "jane") %anyin% c("jill", "bill")
#> [1] TRUE

1:5 %any!in% c(2, 99)
#> [1] TRUE
c("jack", "jill", "john", "jane") %any!in% c("jill", "bill")
#> [1] TRUE

1:5 %any==% c(5, 4, 3, 2, 1)
#> [1] TRUE
1:5 %any==% 6:10
#> [1] FALSE

1:5 %any!=% 1:5
#> [1] FALSE
1:5 %any!=% c(5, 4, 3, 2, 1)
#> [1] TRUE
## define a variable
sample_data <- c(1, 3, 9, 5, NA, -9)

sample_data %anyc% "( >= 1 & <= 10 ) | == -9"
#> [1] TRUE
sample_data %anyc% "is.na"
#> [1] TRUE

## clean up
rm(sample_data)
## define a variable
sample_data <- c(1, 3, 9, 5, -9)

sample_data %anye% "(-8, 1] | [2, 9)"
#> [1] TRUE

## clean up
rm(sample_data)

c("jack", "jill", "john", "jane") %anygrepl% "^ja"
#> [1] TRUE
c("jack", "jill", "john", "jane") %anygrepl% "zz"
#> [1] FALSE

c("jack", "jill", "john", "jane") %any!grepl% "^ja"
#> [1] TRUE
c("jack", "jill", "john", "jane") %any!grepl% "j"
#> [1] FALSE

c("a:b", "c:d") %anyflipIn% "b:a"
#> [1] TRUE
c("a:b", "c:d") %anyflipIn% "x:y"
#> [1] FALSE
```
