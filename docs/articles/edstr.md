# Vue d’ensemble

Exemple de la détection des fractures de l’extrémité supérieure du fémur
(FESF).

Le script correspondant :

``` r
```

Le contenu suivant explicite brièvement le résultat de chaque fonction
afin de montrer le process global.

Pour plus de détails sur une fonction, se référer à la page
correspondante dans la partie “Articles”.

## 1 Charger le package

Le package [edstr](https://cpd000001.chrul.net/9000) doit être chargé
dans l’environnement.

``` r
library(edstr)
```

## 2 Configuration par défaut

La fonction
[`edstr_config()`](https://cpd000001.chrul.net/9000/reference/edstr_config.md)
définit des paramètres communs aux fonctions suivantes.

``` r
edstr_config(dest_dir = "_demo/ains/data_sample",
             dest_filename = "ains_sample")
```

## 3 Collection des données

Réalise l’importation de la table \[nom\] depuis la base et son
enregistrement selon les paramètres définis dans
[`edstr_config()`](https://cpd000001.chrul.net/9000/reference/edstr_config.md).

``` r
edstr_import(query = "select * from ains",
             load = TRUE)
```

## 4 Nettoyage des données

Réalise le nettoyage du texte présent dans la table importée et son
enregistrement selon les paramètres définis dans
[`edstr_config()`](https://cpd000001.chrul.net/9000/reference/edstr_config.md).

Prend automatiquement en input la table importée avec
[`edstr_import()`](https://cpd000001.chrul.net/9000/reference/edstr_import.md).

``` r
edstr_clean(load = TRUE)
```

## 5 Extraction avec `edstr_extract()`

Réalise une extraction à partir du texte présent dans la table nettoyée
et son enregistrement selon les paramètres définis dans
[`edstr_config()`](https://cpd000001.chrul.net/9000/reference/edstr_config.md).

Prend automatiquement en input la table nettoyée avec
[`edstr_clean()`](https://cpd000001.chrul.net/9000/reference/edstr_clean.md).

``` r
#edstr_extract()
```
