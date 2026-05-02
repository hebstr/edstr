# edstr 0.3.0 (2026-05-02)

## Internal changes

* Code style unified across all R files with `air` formatter.
* Pre-commit hooks added: `air-format` (posit-dev/air-pre-commit v0.9.0) and
  `jarl-check` (etiennebacher/jarl-pre-commit v0.4.0), both scoped to `.R` files.
* `.Rbuildignore` regex patterns fixed (`^air\.toml$`, added `^jarl\.toml$`).
* `.Rprofile` removed (JVM auto-detection via `find` no longer needed).
* `DESCRIPTION`: migrated from legacy `Roxygen`/`RoxygenNote` fields to
  `Config/roxygen2/markdown` and `Config/roxygen2/version`.
* `dplyr::if_any` and `dplyr::slice` added to explicit imports.

# edstr 0.2.0 (2026-03-26)

## Breaking changes

* All CLI messages, error messages, and interactive menus are now in English.

* `edstr_view()`: the `id` argument now defaults to `NULL` instead of
  auto-detecting at call time. The column is still auto-detected internally,
  but this avoids errors when `data` is not yet available at function
  definition.

## New features

* `edstr_extract()` gains an `exclus_auto_token_min` argument (default `10`)
  to control the minimum n-gram size for automatic exclusion heuristics.
  Previously this was hard-coded.

* `edstr_config()` now validates all arguments on input and raises
  informative errors for wrong types or lengths.

* `edstr_clean()` now validates the `replace` argument structure (must be a
  named character vector or list of named character vectors) and checks that
  the text column exists in `data` before processing.

## Documentation

* Complete rewrite of all vignettes in English with expanded examples
  and a pipeline diagram.
* Full roxygen documentation for all exported functions.
* Added `URL` and `BugReports` fields to DESCRIPTION.

# edstr 0.1.0 (2024-02-16)

* Initial release.
