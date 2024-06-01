
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edstr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Le but de edstr est de fournir un ensemble de fonctions réalisant un
process reproductible d’importation, de nettoyage, et d’extraction de
données textuelles de routine.

## Installation

Avec une connexion internet, le package en développement peut être
installé comme suit :

``` r
# install.packages("devtools")
devtools::install_git("https://codeberg.org/hebstr/edstr")
```

Depuis la bulle, le fichier `edstr_0.1.0.zip` situé dans
`G:/Data/_R_/packages` est à installer manuellement à la manière des
autres packages.

``` r
install.packages("G:/Data/_R_/packages/edstr_0.1.0.zip", repos = NULL)
```

## Résumé

img

Une vue d’ensemble du process est détaillée dans `vignette("edstr")`.

### Configuration initiale

`edstr_config()` :

- Définition de la configuration nécessaire aux fonctions exécutées en
  aval (répertoire de destination, nom des fichiers générés, règles de
  nettoyage, concepts, colonne texte).

- Produit une liste en arrière-plan, nommée par défaut `.config`, dont
  les éléments sont appelés dans les fonctions en aval.

- Certains paramètres peuvent être définis directement dans les
  fonctions en aval, cependant il est conseillé de le faire ici car ils
  ne sont pas censés changer d’une étape à l’autre.

### Production

`edstr_import()` :

- Importation de caractéristiques depuis l’entrepôt

- À partir d’une requête SQL (directe ou par appel d’une table) ou
  depuis un fichier .sql.

- Produit un dataframe automatiquement sauvegardé dans un fichier .RData
  :

  - Situé dans le répertoire défini avec `edstr_config()` (argument
    `dest_dir`)
  - Portant le nom défini dans `edstr_config()` (argument
    `dest_filename`) suivi du suffixe `_import`.

`edstr_clean()` :

- Nettoyage d’une colonne texte présent dans un dataframe.

- Prend automatiquement en input le dataframe produit avec la fonction
  `edstr_import()`, par défaut nommé `[nom_du_fichier]_import`.

- Produit un dataframe automatiquement sauvegardé dans un fichier .RData
  :

  - Situé dans le répertoire défini avec `edstr_config()` (argument
    `dest_dir`)
  - Portant le nom défini dans `edstr_config()` (argument
    `dest_filename`) suivi du suffixe `_clean`.

`edstr_extract()` :

- Extraction depuis une colonne texte présent dans un dataframe sur la
  base d’une recherche par mots-clés.

- Prend automatiquement en input le dataframe nettoyé produit avec la
  fonction `edstr_clean()`, par défaut nommé `[nom_du_fichier]_clean`.

- Produit une liste portant le nom défini dans `edstr_config()`
  (argument `dest_filename`) suivi du suffixe `_extract` (contenu
  détaillé dans `vignette("extract")`)

- Produit automatiquement un dossier portant le nom défini dans
  `edstr_config()` (argument `dest_filename`) suivi du suffixe
  `_extract`, situé dans le répertoire défini avec `edstr_config()`
  (argument `dest_dir`) et contenant :

  - La liste sauvegardé dans un fichier .RData
  - Le décompte des termes extraits, enregistrés aux formats .csv et
    .html (si `HTML = TRUE`)
  - Un dataframe constitué des chaines de caractères extraites avec
    caractéristiques associés (e.g., id_entrepot, id_pat, UF), au
    formats .csv et .html (si `HTML = TRUE`).

### Exploration

La fonction `edstr_view()` permet une exploration rapide des données
textuelles depuis la console sur la base d’une recherche par mots-clés.

- Peut prendre en input tout dataframe contenant a minima une colonne
  texte et une colonne identifiant (de préférence une clé primaire).

- Document balisé lisible depuis la console avec surbrillance des
  éléments recherchés.

- Décompte des mots-clés recherchés classé par ordre de fréquence

- Produit automatiquement un dossier portant le nom défini dans
  `edstr_config()` (argument `dest_filename`) suivi du suffixe `_view`,
  situé dans le répertoire défini avec `edstr_config()` (argument
  `dest_dir`) et contenant :

  - Un dataframe consitué des mots-clés recherchés avec l’identifiant
    associé (classiquement l’id_entrepot)
  - Un dataframe constitué du décompte classé par ordre de fréquence
  - Un dataframe constitué de l’échantillon de documents affichés dans
    la console (10 par défaut)

Plus de détails dans `vignette("edstr")`.

## Template

Pour une utilisation classique, un template de script est mis à
disposition à `G:/Data/_R_/packages/edstr_config/template.R`

``` r
library(edstr)

edstr_config(dest_dir = "___",
             dest_filename = "___",
             str = "G:/Data/_R_/edstr_config/str.RData",
             concepts = "___",
             text = "___")

edstr_import(query = "select * from ___",
             connect_dir = "G:/Data/_R_/database_connection",
             user = "w_etudes",
             load = FALSE)

edstr_clean(replace = clean_all,
            load = FALSE)

edstr_extract(sample = 50,
              split = extract_split,
              replace = extract_replace,
              ngrams = 1:5,
              concepts = "___",
              id = "___",
              group = "___")

#--------------------------------------------------------

edstr_view(data = "___",
           str = "___",
           id = "___")
```

## Origine du nom

edstr est la contraction de “EDS” (Entrepôt de Données de Santé) et de
“str”, diminutif de string, car la finalité est le traitement de chaînes
de caractères.
