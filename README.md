
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edstr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

edstr extracts structured variables from unstructured French clinical
free text stored in an EDS (Entrepot de Donnees de Sante, i.e. an
institutional clinical data warehouse). It provides a pipeline that
imports data from an Oracle database, cleans text with regex rules,
tokenises and matches user-defined concepts, filters false positives,
and exports results as Excel, CSV, and RDS files.

## Installation

Install the development version from GitHub:

``` r
#install.packages("pak")
pak::pak("hebstr/edstr")
```

### System dependencies

- **Java JDK** (\>= 8): required by rJava, RJDBC, and DatabaseConnector
  for Oracle JDBC connectivity. On Debian/Ubuntu:
  `sudo apt install default-jdk` then `sudo R CMD javareconf`.
- **Oracle Instant Client**: required for `edstr_import()` to connect to
  an Oracle database.

If you do not need the database import step (`edstr_import()`), you can
still use the cleaning and extraction functions on locally loaded data
frames.

## Pipeline overview

    edstr_config()
          |
          v
    edstr_import()
          |
          v
    edstr_clean()
          |
          +---------> edstr_view()
          |           (interactive, no save)
          v
    edstr_extract()
          |
          v
      .xlsx / .csv / .rds

Each step (except `edstr_view()`) caches its output as an RDS file. If
the file already exists, the caching system either loads it, overwrites
it, or prompts the user, depending on the `edstr_overwrite` option.

`edstr_view()` branches off from cleaned data for interactive pattern
exploration and does not save anything.

## Exported functions

| Function          | Description                                                                                                                    |
|-------------------|--------------------------------------------------------------------------------------------------------------------------------|
| `edstr_config()`  | Set global options: output directory, file prefix, text column, and caching behaviour. Must be called first.                   |
| `edstr_import()`  | Execute a SQL query against an Oracle database and cache the result as RDS.                                                    |
| `edstr_clean()`   | Apply sequential regex replacements to a text column and cache the result.                                                     |
| `edstr_extract()` | Tokenize text, match concepts, filter false positives, re-match against source text, and export results as XLSX, CSV, and RDS. |
| `edstr_view()`    | Interactively search for a regex pattern in text and display match frequencies. Does not save.                                 |

## Quick start

### Configure the pipeline

``` r
library(edstr)

edstr_config(
  edstr_dirname = "output/my_study",
  edstr_filename = "my_study",
  edstr_text = "note_text",
  edstr_overwrite = TRUE
)
```

### Import and clean

``` r
df_import <- edstr_import(
  query = "sql/my_query.sql",
  head = 1000,
  user = "my_user"
)

df_clean <- edstr_clean(
  data = df_import,
  replace = c("\\p{Zs}{2,}" = " ", "\\n" = " ")
)
```

### Explore patterns

Use `edstr_view()` to iterate on regex patterns before extraction.

``` r
edstr_view(
  data = df_clean,
  pattern = "fractur",
  ngrams = 3
)
```

### Extract structured variables

**Flat concepts** — a named character vector where each element is an
independent concept:

``` r
result <- edstr_extract(
  data = df_clean,
  concepts = c(fracture = "fractur", femur = "f(e|e)mur|fesf"),
  token = c(1, 2),
  group = "id_pat"
)
```

**Nested concepts** — a named list grouping sub-concepts under a root:

``` r
result <- edstr_extract(
  data = df_clean,
  concepts = list(
    fracture = c(
      fesf = "fesf|extremite superieure",
      col = "col (du )?femur"
    )
  ),
  group = "id_pat",
  exclus_manual = "ancienne fracture|fracture ouverte"
)
```

## Key features

**Concept matching.** Concepts are named regex patterns defining
clinical entities. A named character vector creates independent
concepts; a nested named list groups sub-concepts under a root.

**Collapse and intersect modes.** `collapse = TRUE` OR-combines all
patterns into a single regex. `intersect = TRUE` keeps only documents
matching all root-level concepts.

**False-positive filtering.** Manual exclusions via a user-supplied
regex (`exclus_manual`) and automatic heuristics on long tokens
(`exclus_auto_token_min`).

**Source re-matching.** After token-level matching on
ASCII-transliterated n-grams, patterns are re-matched against the
original text with accent normalisation. Discrepancies between the two
are flagged as mismatches for review.

**Built-in caching.** Every pipeline step writes an RDS file. The
`edstr_overwrite` option (`TRUE` / `FALSE` / `NULL`) controls whether
existing files are overwritten, loaded silently, or trigger an
interactive prompt.

## Output

`edstr_extract()` returns a nested list and saves three files:

| File    | Contents                                                                                             |
|---------|------------------------------------------------------------------------------------------------------|
| `.xlsx` | Excel workbook with one sheet per result type (extraction, counts, exclusions, mismatch, parameters) |
| `.csv`  | Flat extraction table for downstream analysis                                                        |
| `.rds`  | Full nested list with all intermediate objects, reloaded by the caching system                       |

## Vignettes

Detailed documentation is available in six vignettes:

- Get started (`vignette("edstr")`)
- Pipeline configuration (`vignette("config")`)
- Data import (`vignette("import")`)
- Text cleaning (`vignette("clean")`)
- Text extraction (`vignette("extract")`)
- Interactive exploration (`vignette("explore")`)

## Contributing

Bug reports and feature requests:
<https://github.com/hebstr/edstr/issues>

Source code: <https://github.com/hebstr/edstr>

## License

GPL (\>= 3)
