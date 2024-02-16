
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edstr

<!-- badges: start -->
<!-- badges: end -->

The goal of edstr is to …

## Installation

You can install the development version of edstr from
[Codeberg](https://codeberg.org/hebstr/edstr) like so:

``` r
# install.packages("devtools")
devtools::install_git("https://codeberg.org/hebstr/edstr")
```

## Example

``` r
edstr_config(dest_dir = "~/Documents/pro/data/fracture2",
             dest_filename = "urgtest",
             str = "~/Documents/pro/~des/$eds/config/str.RData",
             concepts = "~/Documents/pro/~des/$eds/config/concepts.RData")
#> 
#> ── edstr_config ────────────────────────────────────────────────────────────────
#> 
#> ℹ working directory: /home/julien/Documents/pro/~r_pkg/edstr
#> 
#> ℹ destination
#> • existing directory: '~/Documents/pro/data/fracture2'
#> • filename: urgtest
#> 
#> ℹ configuration file: .config
#> 
#> ────────────────────────────────────────────────────────────────────────────────
```
