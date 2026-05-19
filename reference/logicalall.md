# Several ways to evaluate whether all values meet logical conditions including logical range comparison helpers

Several ways to evaluate whether all values meet logical conditions
including logical range comparison helpers

## Usage

``` r
e1 %agele% e2

e1 %agel% e2

e1 %agle% e2

e1 %agl% e2

e1 %age% e2

e1 %ag% e2

e1 %ale% e2

e1 %al% e2

e1 %ain% e2

e1 %a!in% e2

e1 %anin% e2

e1 %a==% e2

e1 %a!=% e2

e1 %ac% e2

e1 %ae% e2

e1 %agrepl% e2

e1 %a!grepl% e2
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

A logical value whether all `e1` meet the logical conditions.

## Examples

``` r

1:5 %agele% c(2, 4)
#> [1] FALSE
1:5 %agele% c(4, 2) # order does not matter uses min / max
#> [1] FALSE

1:5 %agel% c(2, 4)
#> [1] FALSE
1:5 %agel% c(4, 2) # order does not matter uses min / max
#> [1] FALSE

1:5 %agle% c(2, 4)
#> [1] FALSE
1:5 %agle% c(4, 2) # order does not matter uses min / max
#> [1] FALSE

1:5 %agl% c(2, 4)
#> [1] FALSE
1:5 %agl% c(4, 2) # order does not matter uses min / max
#> [1] FALSE

1:5 %age% 2
#> [1] FALSE
1:5 %age% 4
#> [1] FALSE

1:5 %ag% 2
#> [1] FALSE
1:5 %ag% 4
#> [1] FALSE

1:5 %ale% 2
#> [1] FALSE
1:5 %ale% 4
#> [1] FALSE

1:5 %al% 2
#> [1] FALSE
1:5 %al% 4
#> [1] FALSE

1:5 %ain% c(2, 99)
#> [1] FALSE
c("jack", "jill", "john", "jane") %ain% c("jill", "jane", "bill")
#> [1] FALSE

1:5 %a!in% c(2, 99)
#> [1] FALSE
c("jack", "jill", "john", "jane") %a!in% c("jill", "jane", "bill")
#> [1] FALSE

1:5 %a==% 1:5
#> [1] TRUE
1:5 %a==% 5:1
#> [1] FALSE

1:5 %a!=% 1:5
#> [1] FALSE
1:5 %a!=% 5:1
#> [1] FALSE
1:5 %a!=% c(5, 4, 1, 3, 2)
#> [1] TRUE
## define a variable
sample_data <- c(1, 3, 9, 5, NA, -9)

## suppose that we expect that values should fall in [1, 10]
## unless they are special character, -9 used for unknown / refused
sample_data %ac% "( >= 1 & <= 10 ) | == -9"
#> [1] NA

## we might expect some missing values and be OK as long as
## above conditions are met or values are missing
sample_data %ac% "( >= 1 & <= 10 ) | == -9 | is.na"
#> [1] TRUE

## equally we might be expecting NO missing values
## and want missing values to come up as FALSE
sample_data %ac% "(( >= 1 & <= 10 ) | == -9) & !is.na"
#> [1] FALSE

## clean up
rm(sample_data)
## define a variable
sample_data <- c(1, 3, 9, 5, -9)

sample_data %ae% "(-8, 1] | [2, 9)"
#> [1] FALSE
sample_data %ae% "(-Inf, Inf)"
#> [1] TRUE

## clean up
rm(sample_data)

c("jack", "jane", "ajay") %agrepl% "ja"
#> [1] TRUE
c("jack", "jill", "john", "jane", "sill", "ajay") %agrepl% "^ja"
#> [1] FALSE

c("jack", "jane", "ajay") %a!grepl% "ja"
#> [1] FALSE
c("jack", "jill", "john", "jane", "sill", "ajay") %a!grepl% "^ja"
#> [1] FALSE
c("jack", "jill", "john", "jane", "sill", "ajay") %a!grepl% "ja$"
#> [1] TRUE
```
