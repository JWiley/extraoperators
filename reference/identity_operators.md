# Strict Identity Operators

These operators are wrappers around
[`identical()`](https://rdrr.io/r/base/identical.html) for whole-object
identity checks. They return a single logical value and are not
elementwise comparisons.

## Usage

``` r
e1 %===% e2

e1 %!==% e2
```

## Arguments

- e1:

  An object to compare.

- e2:

  An object to compare.

## Value

A single logical value.

## Examples

``` r

1:3 %===% 1:3
#> [1] TRUE
1 %===% 1L
#> [1] FALSE

1:3 %!==% 3:1
#> [1] TRUE
1 %!==% 1L
#> [1] TRUE
```
