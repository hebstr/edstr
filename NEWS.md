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
