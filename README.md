
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R2Word

<!-- badges: start -->
<!-- badges: end -->

R2Word, an R package that uses `flextable` functions to quickly and
easily creating MS Word tables.

## Description

Often R dataframes, matrices, or tables need to be printed out to a MS
Word file that can then be moved into a slide deck. This sort of ad-hoc
work requires a fast and easy transfer from R to MS Word. This is a
small (3 functions) package that helps you do this. It mostly consists
of using the `flextable` and `officeR` packages.

## Installation

You can install the released version of R2Word from github:

``` r
install.packages('devtools')
library(devtools)
devtools::install_github("CJangelo/R2Word")
```

This package probability won’t be released to CRAN, but this github
repository will be updated due to how frequently me and my team use
these functions.

## Examples

For concise examples of how the functions can help your workflow, see
the Introductory Vignette
(<https://cjangelo.github.io/R2Word/articles/introduction.html>) under
`Articles` at the top of this page.

## License

This R package is free and open source software (License: GPL (&gt;=
3)).
