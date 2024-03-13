
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edstr

<!-- badges: start -->
<!-- badges: end -->

The goal of edstr is to …

## Installation

You can install the development version of edstr from Codeberg like so:

``` r
# install.packages("devtools")
devtools::install_git("https://codeberg.org/hebstr/edstr")
```

## Example

``` r
edstr_config(dest_dir = "../data/fracture",
             dest_filename = "gs",
             str = "config/str.RData",
             concepts = "config/concepts.RData",
             text = "text")
```
