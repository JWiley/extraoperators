# Chain Operator

This operator allows operators on the right hand side to be chained
together. The intended use case is when you have a single object on
which you want to perform several operations. For example, testing
whether a variable is between 1 and 5 or equals special number 9, which
might be used to indicate that someone responded to a question (i.e.,
its not missing per se) but that they preferred not to answer or did not
know the answer.

## Usage

``` r
e1 %c% e2
```

## Arguments

- e1:

  The values to be operated on, on the left hand side

- e2:

  One non-empty character string (it MUST be quoted) containing the
  operators and values to apply to \`e1\`. Operators can be chained
  together using either \`\|\` or \`&\`; these connectors must appear
  between complete conditions, not at the start or end of the string,
  and cannot be doubled. Parentheses are also supported and work as
  expected. See examples for more information on how this function is
  used.

## Value

a logical vector

## Details

\` \`is.na\`, \`!is.na\`, \`is.nan\`, and \`!is.nan\`. These do not need
any values supplied but they work as expected to add those logical
assessments into the chain of operators.

## Examples

``` r

## define a variable
sample_data <- c(1, 3, 9, 5, NA, -9)

## suppose that we expect that values should fall in [1, 10]
## unless they are special character, -9 used for unknown / refused
sample_data %c% "( >= 1 & <= 10 ) | == -9"
#> [1] TRUE TRUE TRUE TRUE   NA TRUE

## we might expect some missing values and be OK as long as
## above conditions are met or values are missing
sample_data %c% "( >= 1 & <= 10 ) | == -9 | is.na"
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE

## equally we might be expecting NO missing values
## and want missing values to come up as FALSE
sample_data %c% "(( >= 1 & <= 10 ) | == -9) & !is.na"
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE

c(1, 3, 9, 5, NA, -9) %c% "is.na & (( >= 1 & <= 10 ) | == -9)"
#> [1] FALSE FALSE FALSE FALSE    NA FALSE

## clean up
rm(sample_data)
```
