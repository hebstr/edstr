
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edstr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Le but de edstr est de fournir un ensemble de fonctions facilitant le
process d’importation, de nettoyage, puis d’extraction de données
textuelles de routine sur la base d’une recherche de mots ou expressions
clés.

## Installation

Le package en développement peut être installé depuis Codeberg comme
suit :

``` r
# install.packages("devtools")
devtools::install_git("https://codeberg.org/hebstr/edstr")
```

## Résumé

``` r
library(edstr)
```

### Process

4 fonctions principales, chacune correspondant à une étape du process.

- `edstr_config()` :

- `edstr_import()` : Importation des caractéristiques depuis l’entrepôt.
  Elle peut se faire depuis une requête SQL (directe ou appelant une
  table) ou depuis un fichier texte contenant une requête SQL.

- `edstr_clean()` :

- `edstr_extract()` :

Chaque étape est détaillée dans `vignette("start")`.

### Exploration du texte

La fonction `edstr_view()`

Elle ne fait pas partie du circuit et n’a donc pas de dépendance au
fichier de configuration généré par `edstr_config()`.

Elle peut prendre comme input n’importe quelle table, du moment que
celle-ci contient a minima une colonne identifiant et une colonne texte.

Plus de détail dans `vignette("start")`.
