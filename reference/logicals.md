# Several logical range comparison helpers

Several logical range comparison helpers

## Usage

``` r
e1 %gele% e2

e1 %gel% e2

e1 %gle% e2

e1 %gl% e2

e1 %g% e2

e1 %ge% e2

e1 %l% e2

e1 %le% e2

e1 %!in% e2

e1 %nin% e2

e1 %flipIn% e2

e1 %grepl% e2

e1 %!grepl% e2
```

## Arguments

- e1:

  A vector to be evaluated.

- e2:

  The right hand side value for the operator. For range operators, use a
  length-two vector without missing values. For `%!in%`, use a non-empty
  lookup vector. For `%c%`, use one non-empty character string
  containing chained comparisons joined by `&` or `|`. For `%e%`, use
  one non-empty character string containing interval notation joined by
  `&` or `|`. For `%grepl%`, use one non-empty regular expression.

## Value

A logical vector of the same length as `e1`.

## Examples

``` r

1:5 %gele% c(2, 4)
#> [1] FALSE  TRUE  TRUE  TRUE FALSE
1:5 %gele% c(4, 2) # order does not matter uses min / max
#> [1] FALSE  TRUE  TRUE  TRUE FALSE

1:5 %gel% c(2, 4)
#> [1] FALSE  TRUE  TRUE FALSE FALSE
1:5 %gel% c(4, 2) # order does not matter uses min / max
#> [1] FALSE  TRUE  TRUE FALSE FALSE

1:5 %gle% c(2, 4)
#> [1] FALSE FALSE  TRUE  TRUE FALSE
1:5 %gle% c(4, 2) # order does not matter uses min / max
#> [1] FALSE FALSE  TRUE  TRUE FALSE

1:5 %gl% c(2, 4)
#> [1] FALSE FALSE  TRUE FALSE FALSE
1:5 %gl% c(4, 2) # order does not matter uses min / max
#> [1] FALSE FALSE  TRUE FALSE FALSE

1:5 %g% c(2)
#> [1] FALSE FALSE  TRUE  TRUE  TRUE

1:5 %ge% c(2)
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE

1:5 %l% c(2)
#> [1]  TRUE FALSE FALSE FALSE FALSE

1:5 %le% c(2)
#> [1]  TRUE  TRUE FALSE FALSE FALSE

1:5 %!in% c(2, 99)
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE
c("jack", "jill", "john", "jane") %!in% c("jill", "jane", "bill")
#> [1]  TRUE FALSE  TRUE FALSE

c("jack", "jill", "john", "jane", "sill", "ajay") %grepl% "ja"
#> [1]  TRUE FALSE FALSE  TRUE FALSE  TRUE
c("jack", "jill", "john", "jane", "sill", "ajay") %grepl% "^ja"
#> [1]  TRUE FALSE FALSE  TRUE FALSE FALSE

c("jack", "jill", "john", "jane", "sill", "ajay") %!grepl% "ja"
#> [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE
c("jack", "jill", "john", "jane", "sill", "ajay") %!grepl% "^ja"
#> [1] FALSE  TRUE  TRUE FALSE  TRUE  TRUE
```
