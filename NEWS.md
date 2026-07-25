# edstr (development version)

## Bug fixes

- `edstr_extract()` no longer searches documents that have no source text.
  A `NA` source was formatted into the string `"NA"`, which reached tokenisation as the token `na` and could be matched by a concept, so a document reported as `unmatched$no_source` could also surface as a match.

- `edstr_extract()` no longer reports excluded tokens as source mismatches.
  Tokens dropped by `exclus_manual` or the automatic heuristics were compared against a source table built after those exclusions, so every one of them landed in `mismatched` with the warning "token matched, not confirmed in source".

- `edstr_extract()` rejects a `data` column whose name the output builds for itself.
  A column named `extract` or `id_group` was overwritten without a word, and one named like a concept key was silently split into `<key>.x` and `<key>.y`; `n` and `concept` aborted deep in the assembly with an opaque message once the whole pipeline had run.

- `edstr_extract()` reports a multi-pattern concept root the same way whatever the root count.
  The check ran after the concepts were parsed, and parsing survived that input on a single root only, through column recycling; two or more roots aborted earlier with `Tibble columns must have compatible sizes`.

- `edstr_extract()` normalises sub-concept names given as a named character vector.
  Only list names were lowercased and stripped, so `list(cancer = c("Sein Droit" = "sein"))` reached the `note` output as `class='extract Sein Droit'`, which a browser reads as two separate classes.

- `edstr_extract()` applies `ano_hash` and `ano_hide` to every output.
  The anonymised frame was built inside the text formatter and then reduced to the key and text columns, which dropped every column it had transformed before anything read it, while every output kept deriving from the untouched input: `ano_hash = "ipp"` left the column in clear in `data$base`, `data$extract`, `data$note`, the Excel sheets and the `gt` tables, with the documentation promising the opposite.
  Anonymisation now runs once on the input frame, before extraction.
  Both arguments are matched case-insensitively (`ano_hash` was case-sensitive, so a pattern could silently miss a capitalised column), the hash keeps 16 hexadecimal characters instead of 7 (28 bits collided with probability \~12% over 8000 rows), and a pattern matching no column, or matching `text_input`, now aborts instead of passing silently.
  Both also read their pattern in the same regex dialect: `ano_hide` was applied through `grep()`, so a construct such as a lookahead passed validation and then failed inside `dplyr` with `invalid regular expression`.

- `edstr_extract()` rejects an `ano_hash` or `ano_hide` pattern that matches the `id` or `group` column.
  Anonymising a key desynchronised the tokenised frame from the one carrying the results: pointing `ano_hide` at the document id reported every document as a true negative, with no error and no warning.

- `edstr_extract()` no longer reports an apostrophe separator as a source mismatch.
  Source matching accepts an apostrophe wherever a token holds a space, but the mismatch check normalised only hyphens and `<br/>`, so a span such as `l' aorte` was confirmed in the source and then reported as unconfirmed anyway.

- `edstr_extract()` names the default concept `concepts` on every path.
  An unnamed single concept produced the key `concept`, which collided with the `concept` output column and aborted the pipeline with `Can't select columns that don't exist`; a collapsed flat set produced the literal key `<concept>`, which surfaced as a column name and as a malformed CSS class in the `note` output.

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

- `edstr_extract(collapse = TRUE)` now collapses a nested list of named
  character vectors instead of deparsing it.
  A `concepts` list such as `list(cancer = c(sein = "sein", poumon = "poumon"))`
  used the R source of each vector as its pattern, so the concept matched
  nothing.
  Only a list of lists collapsed correctly.

- `edstr_extract()` now aborts with an actionable message when a concept root
  groups several sub-patterns as a named vector under `collapse = FALSE`,
  instead of failing deep in tokenisation with a cryptic recycling error.
  Track sub-concepts separately with a nested list,
  `list(cancer = list(sein = "sein", poumon = "poumon"))`, which the
  documented examples now use.

- `edstr_extract(intersect = TRUE)` now rejects a single root that carries
  several sub-concepts, instead of silently ignoring the intersection.
  The guard counts roots, not concept keys, so a nested single-root input no
  longer slips through.

- `edstr_extract(collapse = TRUE)` now collapses a set of single-pattern nested
  lists instead of deparsing it.
  `list(cancer = list(sein = "sein"), diab = list(t2 = "diabet"))` leaves every
  root at length one, which took the flat branch and used the R source of each
  inner list as its pattern.
  The regex stayed syntactically valid, so the run ended on `No matches found`
  with nothing else to go on.

- A `.sql` file passed to `edstr_import(query = )` keeps the comment markers
  that sit inside a string literal.
  Comment stripping ran on the raw text, so `LIKE '%--%'` was truncated to
  `LIKE '%` and `nom != '---'`, the package's own default hide pattern, to
  `nom != '`.
  Those break loudly on an unbalanced quote, but a block marker spanning two
  literals did not: `SELECT '/*' AS a, 'x' AS b, '*/' AS c FROM t` was sent as
  `SELECT '' AS c FROM t`, valid SQL over a different result set.

- An empty Excel sheet keeps a uniform header row.
  `wb_dims(select = "data")` collapses onto the header on a zero-row frame, and
  the concept and text colouring was the one data-scoped style left ungated, so
  it repainted the header cells of the columns it targets.
  A colour naming a column absent from an empty sheet also aborted the whole
  workbook build with `Column exceeds valid range`, the normalisation that
  filters such names running only when the frame carried rows.

- The `n` row counter of `data$extract` and `data$note` is an integer.
  It came from `rownames_to_column()`, so it was text: the delivered Excel sheet
  and the `gt` table sorted it lexicographically, putting row `10` before row
  `2`.

- `edstr_extract()` reports a multi-pattern concept at any nesting depth.
  The guard inspected the top level only, so its own hint, wrapping sub-concepts
  in `list()`, led straight to a shape it no longer covered:
  `list(cancer = list(sein = c("sein", "mammaire")))` reached the `regex_df`
  build and died on `Tibble columns must have compatible sizes`.
  The abort now names the flattened concept, `cancer_sein`.

- `edstr_extract(ano_hash = )` keeps missing values missing.
  Every `NA` in a hashed column was hashed like any other value and came back as
  one shared 16-character pseudonym, so completeness reporting saw no missing
  value left and, worse, a join on the pseudonymised column matched all the
  identity-unknown rows with one another, merging distinct records into a single
  pseudo-patient.
  `id` and `group` are out of reach of the pattern, but any secondary
  identifier is not.

- `edstr_extract(intersect = TRUE)` now counts a document matching some roots
  but not all as a non-case, instead of dropping it from every output.
  Such a document was absent from `data$extract` and from all four `unmatched`
  buckets at once, so the counts did not close and `n_no_concept`, which the
  documentation designates as the denominator, was understated: a prevalence
  over three documents where one matched both roots, one matched a single root
  and one matched neither came out at 1/2 instead of 1/3.
  It now lands in `no_concept`, the unit under `intersect = TRUE` being the
  compound concept.
  `data$match` stays pre-intersect, so
  `anti_join(data$match, data$extract, by = id)` still recovers the set.
  Results stored from an earlier `intersect = TRUE` run are not comparable.

- `edstr_extract()` rejects two concepts that share a sub-name.
  `list(cancer = c(sein = "mammaire"), benin = c(sein = "adenofibrome"))` kept
  the roots apart in the dummy columns but labelled both `sein`, and the
  sub-name is what groups the source passes, so the two concepts were matched
  as one and the `concept` column contradicted the dummy columns on the same
  row.

## Performance

- `edstr_extract()` is about 40% faster, with identical output.
  The highlight markup built for the `note` output now runs only over the
  documents a concept actually matched, and the `Latin-ASCII` transliteration
  applied to the source is reproduced on a faster Unicode fold.

- The token-matching stage joins the occurrence table once instead of once per
  concept, about 4 times faster at every `token` value.
  The gain grows with the number of n-gram sizes searched: `token = c(1, 2, 3)`
  goes from about 21 seconds to 5 on the reference corpus.

- The automatic exclusion scan is skipped when no n-gram reaches
  `exclus_auto_token_min`, which is what its default of `10` means in practice.
  The scan used to run in full and have its entire output discarded.

- The Excel export builds about 40% faster, with identical output.
  Cell borders were painted over the whole table, which openxlsx2 resolves in
  time that grows super-linearly with the range; the border style is now
  resolved on two prototype rows and broadcast, so the largest sheet no longer
  dominates the build.

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

- `ngrams` in `edstr_view()` is described consistently in the vignettes and the
  reference as the total window size, the matched token included.
  `ngrams = 3` captures the match plus up to two further tokens; the vignettes
  previously read as if all three were captured after the match.

## Breaking changes

- `edstr_extract()`: the `mismatch_data` argument is renamed to
  `unmatched_data`.
  It now gates only the rows of the `unmatched$no_concept` set
  (documents whose text was searched and matched no concept), which can be
  large;
  the count `unmatched$n_no_concept` is always exact regardless, and the
  `no_source`, `empty_text` and `outside_p` sets are always populated.

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
  concept, so `unmatched$n_no_concept` gives a usable denominator (always exact,
  independent of `unmatched_data`).

## Internal changes

- `dplyr (>= 1.1.1)` is now declared, the version that introduced the join
  `relationship` argument.
  The package already required at least 1.1.0 for `pick()` without saying so.

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
