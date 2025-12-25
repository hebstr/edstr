# edstr_extract() : extraction du texte

## 1 blabla

préparation

``` r
edstr_config(dest_dir = "_demo/ains/data_sample",
             dest_filename = "ains_sample",
             replace = "_demo/config/str.RData",
             concepts = "_demo/config/concepts_ains.RData")

edstr_import(query = "select * from ains",
             load = TRUE)

edstr_clean(load = TRUE)
```

## 2 extract

on y va

``` r
edstr_extract(ngrams = 1:2,
              concepts = .config$concepts,
              intersect = TRUE,
              id = "id_entrepot",
              group = "ipp")
```
