roperators
==========

  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/JWiley/roperators/branch/master/graph/badge.svg)](https://codecov.io/gh/JWiley/roperators?branch=master)
  <!-- badges: end -->


An `R` package with some simple operators to help speed up everyday
tasks.

Installation
------------

TO get the latest development version, use:

```
#install.packages("devtools")
devtools::install_github("JWiley/roperators")
```

Overview
--------

The table below summarizes what this package provides. Fundamentally
it provides 10 different logivcal operators, which can be accessed on
their own to return a logical vector, but also prefixed by `?` (which)
to return the **indices** that are `TRUE` or prefixed by `s` (subset)
to return only those **values** that are `TRUE` or prefixed by `a`
(all) to return a single logical value, are ALL true? An entire all
logical comparison can be prefixed by `!` to generate a NONE
evaluation.


| Operator       | What it does                                 | Which?           | Subset           | All              |
|----------------|----------------------------------------------|------------------|------------------|------------------|
| %l%            | Less than                                    | %?l%             | %sl%             | %al%             |
| %le%           | Less than or equal                           | %?le%            | %sle%            | %ale%            |
| %g%            | Greater than                                 | %?g%             | %sg%             | %ag%             |
| %ge%           | Greater than or equal                        | %?ge%            | %sge%            | %age%            |
| %gl%           | Greater than AND less than                   | %?gl%            | %sgl%            | %agl%            |
| %gel%          | Greater than or equal AND less than          | %?gel%           | %sgel%           | %agel%           |
| %gle%          | Greater than AND less than or equal          | %?gle%           | %sgle%           | %agle%           |
| %gele%         | Greater than or equal AND less than or equal | %?gele%          | %sgele%          | %agele%          |
| %in%           | In                                           | %?in%            | %sin%            | %ain%            |
| %!in% OR %nin% | Not in                                       | %?!in% OR %?nin% | %s!in% OR %snin% | %a!in% OR %anin% |


Examples
--------

```
1:5 %nin% c(2, 99)
1:5 %snin% c(2, 99)
1:5 %sin% c(2, 99)
```
