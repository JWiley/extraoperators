# Several ways to return an index based on logical range comparison helpers

Several ways to return an index based on logical range comparison
helpers

## Usage

``` r
e1 %?gele% e2

e1 %?gel% e2

e1 %?gle% e2

e1 %?gl% e2

e1 %?ge% e2

e1 %?g% e2

e1 %?le% e2

e1 %?l% e2

e1 %?in% e2

e1 %?!in% e2

e1 %?nin% e2

e1 %?==% e2

e1 %?!=% e2

e1 %?c% e2

e1 %?e% e2

e1 %?grepl% e2

e1 %?!grepl% e2
```

## Arguments

- e1:

  A vector to be evaluated and for which the indices will be returned.

- e2:

  The right hand side value passed to the underlying logical operator.
  See
  [`logicals`](https://joshuawiley.com/extraoperators/reference/logicals.md),
  [`%c%`](https://joshuawiley.com/extraoperators/reference/grapes-c-grapes.md),
  and
  [`%e%`](https://joshuawiley.com/extraoperators/reference/grapes-e-grapes.md)
  for operator-specific requirements.

## Value

A vector of the indices identifying which values of `e1` meet the
logical conditions.

## Examples

``` r

1:5 %?gele% c(2, 4)
#> [1] 2 3 4
1:5 %?gele% c(4, 2) # order does not matter uses min / max
#> [1] 2 3 4

1:5 %?gel% c(2, 4)
#> [1] 2 3
1:5 %?gel% c(4, 2) # order does not matter uses min / max
#> [1] 2 3

1:5 %?gle% c(2, 4)
#> [1] 3 4
1:5 %?gle% c(4, 2) # order does not matter uses min / max
#> [1] 3 4

1:5 %?gl% c(2, 4)
#> [1] 3
1:5 %?gl% c(4, 2) # order does not matter uses min / max
#> [1] 3

1:5 %?ge% 2
#> [1] 2 3 4 5
1:5 %?ge% 4
#> [1] 4 5

1:5 %?g% 2
#> [1] 3 4 5
1:5 %?g% 4
#> [1] 5

1:5 %?le% 2
#> [1] 1 2
1:5 %?le% 4
#> [1] 1 2 3 4

1:5 %?l% 2
#> [1] 1
1:5 %?l% 4
#> [1] 1 2 3

1:5 %?in% c(2, 99)
#> [1] 2
c("jack", "jill", "john", "jane") %?in% c("jill", "jane", "bill")
#> [1] 2 4

1:5 %?!in% c(2, 99)
#> [1] 1 3 4 5
c("jack", "jill", "john", "jane") %?!in% c("jill", "jane", "bill")
#> [1] 1 3

1:5 %?nin% c(2, 99)
#> [1] 1 3 4 5
c("jack", "jill", "john", "jane") %?nin% c("jill", "jane", "bill")
#> [1] 1 3

11:15 %?==% c(11, 1, 13, 15, 15)
#> [1] 1 3 5

11:15 %?!=% c(11, 1, 13, 15, 15)
#> [1] 2 4
## define a variable
sample_data <- c(1, 3, 9, 5, NA, -9)

## suppose that we expect that values should fall in [1, 10]
## unless they are special character, -9 used for unknown / refused
sample_data %?c% "( >= 1 & <= 10 ) | == -9"
#> [1] 1 2 3 4 6

## we might expect some missing values and be OK as long as
## above conditions are met or values are missing
sample_data %?c% "( >= 1 & <= 10 ) | == -9 | is.na"
#> [1] 1 2 3 4 5 6

## equally we might be expecting NO missing values
## and want missing values to come up as FALSE
sample_data %?c% "(( >= 1 & <= 10 ) | == -9) & !is.na"
#> [1] 1 2 3 4 6

## clean up
rm(sample_data)
## define a variable
sample_data <- c(1, 3, 9, 5, -9)

sample_data %?e% "(-8, 1] | [2, 9)"
#> [1] 1 2 4

## clean up
rm(sample_data)

c("jack", "jill", "john", "jane", "sill", "ajay") %?grepl% "ja"
#> [1] 1 4 6
c("jack", "jill", "john", "jane", "sill", "ajay") %?grepl% "^ja"
#> [1] 1 4

c("jack", "jill", "john", "jane", "sill", "ajay") %?!grepl% "ja"
#> [1] 2 3 5
c("jack", "jill", "john", "jane", "sill", "ajay") %?!grepl% "^ja"
#> [1] 2 3 5 6
```
