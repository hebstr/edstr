# edstr_view() : rechercher dans le texte

## 1 préparation

prep

``` r
edstr_config(dest_dir = "_demo/ains/data_sample",
             dest_filename = "ains_sample")

edstr_clean(load = TRUE)
```

``` r
ls()
```

## 2 Recherche dans le texte

explore

``` r
edstr_view(data = ains_sample_clean,
           str = "cellul\\w*",
           id = "id_entrepot",
           output_sample = 0)
```

assignation

``` r
ls()
```
