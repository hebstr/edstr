# Deferred

Findings DEFERRED lors des code reviews. À revisiter périodiquement.

| Date | Finding | Fichier | Raison du report | Échéance |
|------|---------|---------|-----------------|----------|
| 2026-03-28 | `print()` dans corps de fonctions non-suppressable | R/edstr_view.R, R/edstr_extract.R | Remplacer par du formatting cli demande de repenser l'affichage des structures nested | — |
| 2026-03-28 | `check_id_key` interne exposé dans `\usage` de edstr_extract | R/edstr_extract.R, man/edstr_extract.Rd | Change la signature publique + logique interne, à traiter lors d'un refactoring API | — |
| 2026-03-28 | Pas de tests error-path pour `edstr_extract()` | tests/testthat/ | Tâche dédiée : config manquante, mauvais inputs, concepts invalide, token invalide | — |
