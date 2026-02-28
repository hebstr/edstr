# Nettoyer les données textuelles

Applique des remplacements (regex) sur une colonne de texte via
[`stringr::str_replace_all()`](https://stringr.tidyverse.org/reference/str_replace.html).
Le résultat est enregistré en `.rds` dans le répertoire défini par
[`edstr_config()`](https://cpd000001.chrul.net/9000/reference/edstr_config.md).
Si le fichier existe déjà, l'utilisateur choisit entre charger ou
écraser (voir option `edstr_overwrite`).

## Usage

``` r
edstr_clean(data, text = getOption("edstr_text"), replace)
```

## Arguments

- data:

  Un dataframe/tibble contenant la colonne texte à nettoyer.

- text:

  Nom de la colonne de texte en character. Par défaut,
  `getOption("edstr_text")`.

- replace:

  Vecteur nommé ou liste de vecteurs nommés de remplacements, passés à
  [`stringr::str_replace_all()`](https://stringr.tidyverse.org/reference/str_replace.html).

## Value

Le tibble nettoyé

## Examples

``` r
edstr_config(
  edstr_dirname = "_extra/collect/data",
  edstr_filename = "avc",
  edstr_text = "texte",
  edstr_overwrite = FALSE
)
#> 
#> ── edstr_config ────────────────────────────────────────────────────────────────
#> 
#> ℹ Répertoire parent : /home/julien/Documents/pro/r_pkg/pkg_edstr
#> 
#> ℹ Les fichiers seront enregistrés dans le répertoire _extra/collect/data et nommés avec le préfixe avc
#> 
#> ────────────────────────────────────────────────────────────────────────────────

df <- tibble::tibble(texte = c("la lapin mange des carrotes"))

df_clean <- edstr_clean(
  data = df,
  replace = c("lapin" = "renard")
)
#> 
#> ── edstr_clean ─────────────────────────────────────────────────────────────────
#> 
#> ℹ Chargement du fichier avc_clean
#> ✔ Chargement du fichier avc_clean [21ms]
#> 
#> ✔ Ficher avc_clean chargé depuis _extra/collect/data/avc_clean.rds
#> 
#> ────────────────────────────────────────────────────────────────────────────────

df_clean
#> # A tibble: 1 × 1
#>   texte                       
#>   <chr>                       
#> 1 la renard mange des carrotes
```
