extraoperators
==============

<!-- badges: start -->
[![CRAN_status](https://www.r-pkg.org/badges/version/extraoperators)](https://cran.r-project.org/package=extraoperators)
[![R-CMD-check](https://github.com/JWiley/extraoperators/workflows/R-CMD-check/badge.svg)](https://github.com/JWiley/extraoperators/actions)
[![codecov](https://codecov.io/gh/JWiley/extraoperators/branch/main/graph/badge.svg?token=rVVdlwT3e5)](https://app.codecov.io/gh/JWiley/extraoperators)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

An `R` package with operators to help speed up everyday tasks.

Installation
------------

To get the latest development version, use:

```r
#install.packages("devtools")
devtools::install_github("JWiley/extraoperators")
```

Otherwise to get from CRAN use:

```r
install.packages("extraoperators")
```

Overview
--------

The table below summarizes what this package provides. Fundamentally
it provides or expands on several logical operators, which can be accessed on
their own to return a logical vector, but also prefixed by `?` (which)
to return the **indices** that are `TRUE` or prefixed by `s` (subset)
to return only those **values** that are `TRUE` or prefixed by `a`
(all) to return a single logical value, are ALL true? They can also be
prefixed by `any` to return a single logical value, is ANY true? An entire
all logical comparison can be prefixed by `!` to generate a NONE
evaluation.


| Operator       | What it does                                 | Which?           | Subset           | All              | Any                |
|----------------|----------------------------------------------|------------------|------------------|------------------|--------------------|
| ==             | Are values / vectors equal                   | %?==%            | %s==%            | %a==%            | %any==%            |
| !=             | Are values / vector NOT equal                | %?!=%            | %s!=%            | %a!=%            | %any!=%            |
| %l%            | Less than                                    | %?l%             | %sl%             | %al%             | %anyl%             |
| %le%           | Less than or equal                           | %?le%            | %sle%            | %ale%            | %anyle%            |
| %g%            | Greater than                                 | %?g%             | %sg%             | %ag%             | %anyg%             |
| %ge%           | Greater than or equal                        | %?ge%            | %sge%            | %age%            | %anyge%            |
| %gl%           | Greater than AND less than                   | %?gl%            | %sgl%            | %agl%            | %anygl%            |
| %gel%          | Greater than or equal AND less than          | %?gel%           | %sgel%           | %agel%           | %anygel%           |
| %gle%          | Greater than AND less than or equal          | %?gle%           | %sgle%           | %agle%           | %anygle%           |
| %gele%         | Greater than or equal AND less than or equal | %?gele%          | %sgele%          | %agele%          | %anygele%          |
| %in%           | In                                           | %?in%            | %sin%            | %ain%            | %anyin%            |
| %!in% OR %nin% | Not in                                       | %?!in% OR %?nin% | %s!in% OR %snin% | %a!in% OR %anin% | %any!in% OR %anynin% |
| %c%            | Chain operations on the RHS together         | %?c%             | %sc%             | %ac%             | %anyc%             |
| %e%            | Set operator, to use set notation            | %?e%             | %se%             | %ae%             | %anye%             |
| %grepl%        | does text match a regular expression         | %?grepl%         | %sgrepl%         | %agrepl%         | %anygrepl%         |
| %!grepl%       | does text NOT match a regular expression     | %?!grepl%        | %s!grepl%        | %a!grepl%        | %any!grepl%        |
| %flipIn%       | In, ignoring order around a colon            | --               | --               | --               | %anyflipIn%        |
| %===%          | Are objects strictly identical               | --               | --               | --               | --                 |
| %!==%          | Are objects not strictly identical           | --               | --               | --               | --                 |

Using these operators, you can accomplish many different tasks by
just remembering the additional prefixes: `?` for `which()`, `a` for
`all()`, `any` for `any()`, and `s` for `subset()`. While simple, this can save quite a
few keystrokes and make various logical comparisons less convoluted.
