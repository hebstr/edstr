# edstr (development version)

## Bug fixes

- `edstr_extract()` now pairs each n-gram size with its own tokenised text.
  A `token` vector that was not consecutive from `1` either matched against the
  wrong n-gram size without warning, as with `token = c(2, 1)`, or failed with
  `subscript out of bounds`, as with `token = c(1, 3)`.

- `edstr_extract()` no longer highlights its own markup in the `note` output.
  A concept matching a word the markup itself uses, such as `extract`, `span`,
  `class`, or another concept's name, produced nested and malformed spans.

- `edstr_extract()` now matches terms written with a ligature.
  `cœur`, `œdème`, `œsophage` and the like matched at the token level but could
  never be confirmed against the source, so they were reported as `mismatched`
  and dropped from the extraction.
  Both the ligature and the two-letter spelling are now found, and each is
  returned as written in the source.

## Performance

- `edstr_extract()` is about a third faster, with identical output.
  The highlight markup built for the `note` output now runs only over the
  documents a concept actually matched, and the `Latin-ASCII` transliteration
  applied to the source is reproduced on a faster Unicode fold.

- `edstr_extract()` now matches the source text with the `re2` engine instead of
  ICU, which runs the concept alternation in a single linear pass.
  Matches are located on a `Latin-ASCII` copy of the source, the same
  transliteration used for tokenisation, then sliced from the original so
  extracted text keeps its accents, ligatures and case unchanged.
  `re2` is a new hard dependency.

## Documentation

- New `vignette("matching")` explains the two matching stages, why concept
  patterns are written without accents, and how the accented source form is
  recovered.

- `exclus_auto_token_min` is now documented as what it is: a threshold measured in
  n-gram sizes, whose default of `10` sits above every realistic `token` value and so
  disables automatic exclusion entirely.
  The scan runs whatever the threshold is, so the default pays its cost without keeping
  its result.
  Lower it below the smallest n-gram size of interest to enable the heuristic.

## Breaking changes

- `edstr_extract()`: the `mismatch_data` argument is renamed to
  `unmatched_data`.
  It now gates only the `unmatched$no_concept` set
  (documents whose text was searched and matched no concept), which can be
  large;
  the `no_source`, `empty_text` and `outside_p` sets are always populated.

- `edstr_extract()`: the `mismatch` output element (a list of `id` and
  `regex`) is replaced by two top-level elements.
  `unmatched` holds
  `id`/`group` tibbles split by reason (`no_concept`, `no_source`,
  `empty_text`, `outside_p`); `mismatched` holds the token vs source
  discrepancies (formerly `mismatch$regex`).

- The XLSX `mismatch` sheet is renamed to `mismatched`, and a new
  `unmatched` sheet with a `reason` column is added.

- RDS cache files written by earlier versions have an incompatible
  structure and must be deleted or regenerated.

## New features

- `edstr_extract()` now reports documents that produced no matchable text
  in the summary, split into empty or missing sources, sources holding no text
  once markup is stripped, and text the formatter could not extract.
  This replaces the previous `cli_warn()` warning for empty-after-formatting
  documents, which is no longer emitted.

- `edstr_extract()` separates documents that could not be evaluated from true
  negatives.
  A document whose source is empty or `NA` is reported as `unmatched$no_source`
  instead of being counted with the documents that were searched and matched no
  concept, so `unmatched$no_concept` is usable as a denominator.

## Internal changes

- `RJDBC` is dropped from `Imports`.
  Oracle connections go through `DatabaseConnector`, which loads the JDBC driver
  itself, so the declaration had no call site.
  Java (>= 8) is still required, by `rJava` and `DatabaseConnector`.

# edstr 0.3.0 (2026-05-02)

## Breaking changes

- `edstr_import()` and `edstr_clean()` now save cache files as Parquet (`.parquet`) instead of RDS (`.rds`).
  Existing `.rds` cache files from earlier versions will not be detected and must be deleted or regenerated.

- `edstr_extract()` no longer saves a `.csv` file.
  Results are now saved as `.xlsx`, `.json`, and `.rds`.

## Internal changes

- Code style unified across all R files with `air` formatter.
- Pre-commit hooks added: `air-format` (posit-dev/air-pre-commit v0.9.0) and
  `jarl-check` (etiennebacher/jarl-pre-commit v0.4.0), both scoped to `.R` files.
- `.Rbuildignore` regex patterns fixed (`^air\.toml$`, added `^jarl\.toml$`).
- `.Rprofile` removed (JVM auto-detection via `find` no longer needed).
- `DESCRIPTION`: migrated from legacy `Roxygen`/`RoxygenNote` fields to
  `Config/roxygen2/markdown` and `Config/roxygen2/version`.
- `dplyr::if_any` and `dplyr::slice` added to explicit imports.

# edstr 0.2.0 (2026-03-26)

## Breaking changes

- All CLI messages, error messages, and interactive menus are now in English.

- `edstr_view()`: the `id` argument now defaults to `NULL` instead of
  auto-detecting at call time.
  The column is still auto-detected internally,
  but this avoids errors when `data` is not yet available at function
  definition.

## New features

- `edstr_extract()` gains an `exclus_auto_token_min` argument (default `10`)
  to control the minimum n-gram size for automatic exclusion heuristics.
  Previously this was hard-coded.

- `edstr_config()` now validates all arguments on input and raises
  informative errors for wrong types or lengths.

- `edstr_clean()` now validates the `replace` argument structure (must be a
  named character vector or list of named character vectors) and checks that
  the text column exists in `data` before processing.

## Documentation

- Complete rewrite of all vignettes in English with expanded examples
  and a pipeline diagram.
- Full roxygen documentation for all exported functions.
- Added `URL` and `BugReports` fields to DESCRIPTION.

# edstr 0.1.0 (2024-02-16)

- Initial release.
