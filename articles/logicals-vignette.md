# Logical Operators

To start, load the package.

``` r

library(extraoperators)
```

## Logical Comparisons

This section covers basic logical comparisons and shows how they might
be done in base `R` versus using the `extraoperators` package. Many of
these are quite simple, but are defined so that later operators are
possible.

First let’s define our “data” as some numbers stored in
`sample_numbers`.

``` r


sample_numbers <- c(9, 1, 5, 3, 4, 10, 99)
```

Now we can do a series of simple logical comparisons which return a
logical vector of `TRUE` or `FALSE`.

``` r


## base R: greater than 3?
sample_numbers > 3
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE

## base R: greater than or equal to 3?
sample_numbers >= 3
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE

## base R: less than 3?
sample_numbers < 3
#> [1] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE

## base R: less than or equal to 3?
sample_numbers <= 3
#> [1] FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE
```

Unfortunately, we cannot use `<` or `>` in custom operators, so we use
the substitutions: `g = >` and `l = <` and `e = =`. So that `ge = <=`
etc.

``` r


## extraoperators: greater than 3?
sample_numbers %g% 3
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE

## extraoperators: greater than or equal to 3?
sample_numbers %ge% 3
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE

## extraoperators: less than 3?
sample_numbers %l% 3
#> [1] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE

## extraoperators: less than or equal to 3?
sample_numbers %le% 3
#> [1] FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE
```

So far there is no real gain in using `extraoperators` but this changes
for more complex operations. What if we want to know if our values fall
within some range? This is a fairly common task, such as saying that
valid ages must be between 0 and 100 years.

``` r


## base R: greater than 3 and less than 10?
sample_numbers > 3 & sample_numbers < 10
#> [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE

## base R: greater than or equal to 3 and less than 10?
sample_numbers >= 3 & sample_numbers < 10
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE

## base R: greater than 3 and less than or equal to 10?
sample_numbers > 3 & sample_numbers <= 10
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE

## base R: greater than or equal to 3 and less than or equal to 10?
sample_numbers >= 3 & sample_numbers <= 10
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE
```

Base `R` accomplishes this through chaining of operations.
`extraoperators` has built in range operators.

``` r


## extraoperators: greater than 3 and less than 10?
sample_numbers %gl% c(3, 10)
#> [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE

## extraoperators: greater than or equal to 3 and less than 10?
sample_numbers %gel% c(3, 10)
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE

## extraoperators: greater than 3 and less than or equal to 10?
sample_numbers %gle% c(3, 10)
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE

## extraoperators: greater than or equal to 3 and less than or equal to 10?
sample_numbers %gele% c(3, 10)
#> [1]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE
```

Finally, `extraoperators` includes a `not in` operator, `%!in%`.

``` r


## base R: not in 3 or 10
!sample_numbers %in% c(3, 10)
#> [1]  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE

## extraoperators: not in 3 or 10
sample_numbers %!in% c(3, 10)
#> [1]  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
```

The next sections show a few examples of these operators augmented by
prefixes: `?`, `s` and `a`.

## Indices (Which Values?)

Sometimes we want to use a logical comparison and identify indices, such
as to use in a loop. `extraoperators` does this by prefixing operators
with `?` for “which”.

``` r


## base R: what are the indices that match 3 and 10?
which(sample_numbers %in% c(3, 10))
#> [1] 4 6

## extraoperators: what are the indices that match 3 and 10?
sample_numbers %?in% c(3, 10)
#> [1] 4 6

## base R: what are the indices for numbers between 3 and 10?
which(sample_numbers > 3 & sample_numbers < 10)
#> [1] 1 3 5

## extraoperators: what are the indices for numbers between 3 and 10?
sample_numbers %?gl% c(3, 10)
#> [1] 1 3 5
```

This can be readily incorporated in other code for further processing.

## Subsetting

Another fairly common task is selecting only certain observations. For
example, we might want to calculate the average of numbers within a
plausible range (e.g., excluding outliers). In `extraoperators`
subsetting is done by adding an `s` prefix.

``` r


## base R: subset to only numbers between 3 and 10
mean(subset(sample_numbers, sample_numbers > 3 & sample_numbers < 10))
#> [1] 6

## or equivalently 
mean(sample_numbers[sample_numbers > 3 & sample_numbers < 10])
#> [1] 6

## extraoperators: subset to only numbers between 3 and 10
mean(sample_numbers %sgl% c(3, 10))
#> [1] 6
```

Subsetting can be especially useful in quick exploratory analyses.
Graphs are easily hard to read if there are extreme values. Subsetting
makes it fast to “zoom in” on a specific range.

## All (or None)

Finally, you might have some quality controls in place for data. For
example asserting that all ages are between 0 and 100. In
`extraoperators` this is done by adding the prefix `a`.

``` r


## base R: are all numbers between 0 and 10?
all(sample_numbers > 0 & sample_numbers < 10)
#> [1] FALSE

## extraoperators: are all numbers between 0 and 10?
sample_numbers %agl% c(0, 10)
#> [1] FALSE

## extraoperators: are all numbers between 0 and 100?
sample_numbers %agl% c(0, 100)
#> [1] TRUE
```

Sometimes you want to know whether at least one value meets a condition.
In `extraoperators` this is done by using the full `any` prefix.

``` r


## base R: are any numbers in 50 or 99?
any(sample_numbers %in% c(50, 99))
#> [1] TRUE

## extraoperators: are any numbers in 50 or 99?
sample_numbers %anyin% c(50, 99)
#> [1] TRUE

## extraoperators: are any numbers between 90 and 100?
sample_numbers %anygl% c(90, 100)
#> [1] TRUE

## base any() NA handling is preserved
c(NA, 1) %anyg% 2
#> [1] NA

## flip order around a colon before matching
c("a:b", "c:d") %anyflipIn% "b:a"
#> [1] TRUE
```

If you want to know the opposite, are no numbers between 0 and 100, we
can negate the whole operation.

``` r


## extraoperators: are NO numbers between 0 and 100?
!sample_numbers %agl% c(0, 100)
#> [1] FALSE

## extraoperators: are NO numbers between 55 and 60?
!sample_numbers %agl% c(55, 60)
#> [1] TRUE
```

There are also expanded all, subset, and which operators for equals and
not equals.

``` r


## extraoperators: are all values equal?
sample_numbers %a==% sample_numbers
#> [1] TRUE

## extraoperators: are all values NOT equal?
c(1, 3, 5) %a!=% c(5, 1, 3)
#> [1] TRUE

## extraoperators: are objects strictly identical?
sample_numbers %===% sample_numbers
#> [1] TRUE

## identical() is type strict
1 %===% 1L
#> [1] FALSE
1 %!==% 1L
#> [1] TRUE
```

## Chaining

In language, it is fairly natural to make a statement like this: “In my
study, age should be between 18 to 65 and not be missing.” In `R`, the
usual implementation of this is more equivalent to: “In my study, age
should be greater than 18 and age should be less than 65 and age should
not be missing.” `extraoperators` tries to facilitate something closer
to the cleaner original statement using the chaining operator, `%c%`.
The chaining operator chains a set of operations on the right hand side
with the argument on its left hand side passed to each. To accomplish
this, the right hand side must be quoted.

``` r

age <- c(19, 30, 90, 50, NA, 45)
age %c% "(> 18 & < 65) & !is.na"
#> [1]  TRUE  TRUE FALSE  TRUE FALSE  TRUE
```

Because the right hand side of the chaining operator is a character
string that is parsed, it is possible to do some special things in it.
`is.na`, `!is.na`, `is.nan`, and `!is.nan` are special characters that
do not require any further value to work correctly. As shown,
parentheses also work, which allows fine grained control over exactly
what is intended. For example if we expect only adults are in the study,
but those who refused to report their age were coded -9 and people who
failed to complete the questionnaire at all are missing.

``` r

age <- c(19, 30, 90, 50, NA, 16, -9)
age %c% "(> 18 | == -9) & !is.na"
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
```

As with all operators, there are prefixes for all, subset, and which.

``` r

age %ac% "(> 18 | == -9) & !is.na"
#> [1] FALSE

age %ac% "> -Inf"
#> [1] NA
age %ac% "> -Inf & !is.na"
#> [1] FALSE
age %ac% "> -Inf | is.na"
#> [1] TRUE

age %sc% "(> 18 | == -9) & !is.na"
#> [1] 19 30 90 50 -9

age %?c% "(> 18 | == -9) & !is.na"
#> [1] 1 2 3 4 7
```

## Interval Notation Operator

In math, interval notation often is used. For example, we might write:
$`x \in (1, 5) \cup [6, \infty)`$ to indicate that *x* is between the
intervals 1 to 5 (not including 1 or 5) or between 6 and positive
infinity, including 6 but not positive infinity. The interval notation
operator, `%e%` let’s you use fairly similar language in `R`. “\|” is
the union operator and “&” is the intersect operator. Variables are
allowed but no functions as these cannot be parsed.

``` r


c(1, 2, 6, 300) %e% "(1, 5) | [6, Inf)"
#> [1] FALSE  TRUE  TRUE  TRUE

## this is OK
x <- max(mtcars$mpg)
c(1, 2, 6, 300) %e% "(1, 5) | [6, x)"
#> [1] FALSE  TRUE  TRUE FALSE

## ## this would NOT be OK
## c(1, 2, 6, 300) %e% "(1, 5) | [6, max(mtcars$mpg))"
```

## Regular Expressions

Sometimes you want to pattern match. For example, you might want to find
all variable names that match a certain pattern. The
`%grepl% operator can help here, built off`R`'s`grepl\` function.

``` r

## sample dataset
data <- data.frame(
  ID = c(1, 2, 3),
  cesd_1 = c(4, 5, 6),
  cesd_2 = c(7, 8, 9),
  cesd_total = c(11, 13, 15)
)

## find all variables that start with "cesd"
names(data)[grepl("^cesd", names(data))]
#> [1] "cesd_1"     "cesd_2"     "cesd_total"

## or equivalently using grep() with right options
grep("^cesd", names(data), value = TRUE)
#> [1] "cesd_1"     "cesd_2"     "cesd_total"

## here is the operator version
names(data) %sgrepl% "^cesd"
#> [1] "cesd_1"     "cesd_2"     "cesd_total"

## the operator opens up all standard variations
names(data) %?grepl% "^cesd" ## indices
#> [1] 2 3 4
names(data) %s!grepl% "^cesd" ## subset names not in pattern
#> [1] "ID"
names(data) %agrepl% "^cesd" ## do all match the pattern?
#> [1] FALSE
```

An example use case in practice might be to find all variables that are
items in a scale and then calculate the scale score.

``` r

set.seed(123) # Set seed for reproducibility
data <- data.frame(matrix(sample(0:4, 100, replace = TRUE), ncol = 10))
names(data) <- paste0("cesd_", 1:10)
data <- cbind(ID = 1:10, data)

## find all variables that start with "cesd" and end with a number
## use these to sum all the items for the scale score
data$cesd_total <- rowSums(data[, names(data) %sgrepl% "^cesd.*[0-9]$"])

summary(data$cesd_total)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    12.0    18.0    19.0    19.3    20.5    25.0
```

Another example is you load a larger dataset from someone and want to
check whether there are any variable names including spaces. You want to
check that any space, in regular expression captured by “\s”, is not
contained in any variable name. That is all are not in space.

``` r

names(mtcars) %a!grepl% "\\s"
#> [1] TRUE
```

This returns `TRUE` indicating no variable names have a space in them.

Here is an example that fails. At least one variable name has a space.
We follow it up by finding out *which* variable(s). This could be fed
back to the data owners to change, if desired. Lastly, although these
are written as separate codes, when used interactively, one might start
with the test and if it fails change the `a!` to `s` to find the
variables. The intention being just to slightly ease and speed up the
process.

``` r

data <- data.frame(
  a = 1:4,
  `b c` = 5:8, check.names = FALSE
)

names(data) %a!grepl% "\\s" # this fails
#> [1] FALSE

# which variables have spaces?
names(data) %sgrepl% "\\s" 
#> [1] "b c"
```
