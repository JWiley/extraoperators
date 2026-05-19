# Several ways to subset based on logical range comparison helpers

Several ways to subset based on logical range comparison helpers

## Usage

``` r
e1 %sgele% e2

e1 %sgel% e2

e1 %sgle% e2

e1 %sgl% e2

e1 %sge% e2

e1 %sg% e2

e1 %sle% e2

e1 %sl% e2

e1 %sin% e2

e1 %s!in% e2

e1 %snin% e2

e1 %s==% e2

e1 %s!=% e2

e1 %sc% e2

e1 %se% e2

e1 %sgrepl% e2

e1 %s!grepl% e2
```

## Arguments

- e1:

  A vector to be evaluated and subset.

- e2:

  The right hand side value passed to the underlying logical operator.
  See
  [`logicals`](https://joshuawiley.com/extraoperators/reference/logicals.md),
  [`%c%`](https://joshuawiley.com/extraoperators/reference/grapes-c-grapes.md),
  and
  [`%e%`](https://joshuawiley.com/extraoperators/reference/grapes-e-grapes.md)
  for operator-specific requirements.

## Value

A subset of `e1` that meets the logical conditions.

## Examples

``` r

1:5 %sgele% c(2, 4)
#> [1] 2 3 4
1:5 %sgele% c(4, 2) # order does not matter uses min / max
#> [1] 2 3 4

1:5 %sgel% c(2, 4)
#> [1] 2 3
1:5 %sgel% c(4, 2) # order does not matter uses min / max
#> [1] 2 3

1:5 %sgle% c(2, 4)
#> [1] 3 4
1:5 %sgle% c(4, 2) # order does not matter uses min / max
#> [1] 3 4

1:5 %sgl% c(2, 4)
#> [1] 3
1:5 %sgl% c(4, 2) # order does not matter uses min / max
#> [1] 3

1:5 %sge% 2
#> [1] 2 3 4 5
1:5 %sge% 4
#> [1] 4 5

1:5 %sg% 2
#> [1] 3 4 5
1:5 %sg% 4
#> [1] 5

1:5 %sle% 2
#> [1] 1 2
1:5 %sle% 4
#> [1] 1 2 3 4

1:5 %sl% 2
#> [1] 1
1:5 %sl% 4
#> [1] 1 2 3

1:5 %sin% c(2, 99)
#> [1] 2
c("jack", "jill", "john", "jane") %sin% c("jill", "jane", "bill")
#> [1] "jill" "jane"

1:5 %s!in% c(2, 99)
#> [1] 1 3 4 5
c("jack", "jill", "john", "jane") %s!in% c("jill", "jane", "bill")
#> [1] "jack" "john"

1:5 %s==% 1:5
#> [1] 1 2 3 4 5
1:5 %s==% c(1:4, 1)
#> [1] 1 2 3 4

1:5 %s!=% 1:5
#> integer(0)
1:5 %s!=% c(1:4, 1)
#> [1] 5
## define a variable
sample_data <- c(1, 3, 9, 5, NA, -9)

## suppose that we expect that values should fall in [1, 10]
## unless they are special character, -9 used for unknown / refused
sample_data %sc% "( >= 1 & <= 10 ) | == -9"
#> [1]  1  3  9  5 NA -9

## we might expect some missing values and be OK as long as
## above conditions are met or values are missing
sample_data %sc% "( >= 1 & <= 10 ) | == -9 | is.na"
#> [1]  1  3  9  5 NA -9

## equally we might be expecting NO missing values
## and want missing values to come up as FALSE
sample_data %sc% "(( >= 1 & <= 10 ) | == -9) & !is.na"
#> [1]  1  3  9  5 -9

## clean up
rm(sample_data)
## define a variable
sample_data <- c(1, 3, 9, 5, -9)

sample_data %se% "(-8, 1] | [2, 9)"
#> [1] 1 3 5

## clean up
rm(sample_data)

c("jack", "jill", "john", "jane", "sill", "ajay") %sgrepl% "ja"
#> [1] "jack" "jane" "ajay"
c("jack", "jill", "john", "jane", "sill", "ajay") %sgrepl% "^ja"
#> [1] "jack" "jane"

c("jack", "jill", "john", "jane", "sill", "ajay") %s!grepl% "ja"
#> [1] "jill" "john" "sill"
c("jack", "jill", "john", "jane", "sill", "ajay") %s!grepl% "^ja"
#> [1] "jill" "john" "sill" "ajay"
```
