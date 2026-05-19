# Process Sets

This is an internal function and not intended to be used directly. It
processes small sets.

## Usage

``` r
.set1(x, envir)
```

## Arguments

- x:

  A character string

- envir:

  An environment in which to evaluate values

## Value

A data frame with the processed set

## Examples

``` r

## ## below is an example that should generate an (informative) error
## extraoperators:::.set1("(-Inf,x)", envir = environment())

z <- max(mtcars$mpg)
extraoperators:::.set1("(-Inf,z)", envir = environment())
#>   Op1 Val1 Con1 Op2 Val2
#> 1   > -Inf    &   < 33.9

extraoperators:::.set1("(-Inf,30)", envir = environment())
#>   Op1 Val1 Con1 Op2 Val2
#> 1   > -Inf    &   <   30

## clean up
rm(z)
```
