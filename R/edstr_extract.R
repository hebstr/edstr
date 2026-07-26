.extract_save <- \(
  data,
  text_input,
  id,
  group,
  sample,
  seed,
  ano_hash,
  ano_hide,
  ngram_max,
  concepts,
  collapse,
  intersect,
  starts_with_only,
  exclus_manual,
  exclus_auto_escape,
  exclus_auto_token_min,
  regex_replace,
  unmatched_data,
  concept_color,
  text_color,
  save_as_gt,
  save_dir,
  save_files,
  save_extract
) {
  check_class(data, "data.frame")
  check_class(text_input, "character")

  if (
    !is.numeric(ngram_max) ||
      length(ngram_max) != 1L ||
      !is.finite(ngram_max) ||
      ngram_max < 1 ||
      ngram_max != trunc(ngram_max)
  ) {
    cli_abort(c(
      "{.arg ngram_max} must be a single whole number, {.val {1}} or more",
      "x" = "Got {.val {ngram_max}}.",
      "i" = "It is the largest n-gram size tokenised, every smaller size included: {.code ngram_max = 3} searches unigrams, bigrams and trigrams."
    ))
  }

  # alternation belongs inside the pattern: a length > 1 value recycles against
  # the matched rows and silently applies a different pattern to each whenever
  # the two lengths coincide, and any other length dies inside `str_detect()`
  .check_exclus <- \(x, arg) {
    if (!is.null(x) && (!is.character(x) || length(x) != 1L)) {
      cli_abort(c(
        "{.arg {arg}} must be a single regex string or {.code NULL}",
        "x" = "Got {.obj_type_friendly {x}} of length {length(x)}.",
        "i" = "Alternate inside the pattern: {.code \"a|b\"}, not {.code c(\"a\", \"b\")}."
      ))
    }
  }

  .check_exclus(exclus_manual, "exclus_manual")
  .check_exclus(exclus_auto_escape, "exclus_auto_escape")

  # the saved XLSX, RDS and note quote `text_input` verbatim, and `ano_hash` /
  # `ano_hide` do not cover it, so the directory is what keeps a clinical extract
  # off a shared host. Only applied on creation: an existing directory keeps the
  # permissions it was created with
  fs::dir_create(save_dir, mode = "0700")

  cli_save_extract <- map(
    set_names(c("xlsx", "rds", "json")),
    ~ paste("Saving file", fs::path(save_files, ext = .))
  )

  if (!is.null(seed)) {
    local_seed(seed)
  }

  tic.clearlog()
  tic("Full steps")

  cli_h1("edstr_extract")

  ### PARSE CONCEPTS -------------------------------------------------------------

  # before the leaf walk below, which `imap()` skips on an empty input, letting
  # a missing set run four stages deep and surface as "No matches found"
  if (is.null(concepts)) {
    cli_abort(c(
      "{.arg concepts} is not set",
      "i" = "Pass a named pattern, e.g. {.code concepts = c(diabete = \"diabet\")}"
    ))
  }

  # checked before parsing: a multi-pattern leaf breaks the `regex_df` build,
  # and only a single one survives it through recycling. Walked at any depth,
  # since the hint below sends users to `list()`, whose leaves fail the same way
  .multi_leaf <- \(x, path) {
    if (!is.list(x)) {
      return(if (length(x) > 1) path else NULL)
    }

    imap(x, ~ .multi_leaf(.x, paste(path, .y, sep = "_"))) |>
      unlist(use.names = FALSE)
  }

  multi_leaf <- imap(concepts, ~ .multi_leaf(.x, .y)) |> unlist(use.names = FALSE)

  if (!collapse && length(multi_leaf) > 0) {
    cli_abort(c(
      "A concept holds several sub-patterns as a vector, which {.code collapse = FALSE} cannot keep separate.",
      "x" = "Affected concept{?s}: {.val {multi_leaf}}.",
      "i" = "Wrap sub-concepts in {.code list()} to track them separately, or set {.code collapse = TRUE} to OR-combine them."
    ))
  }

  concepts_list <- .timed(
    "Parse concepts",
    .extract_parse_concepts(
      concepts,
      collapse,
      intersect,
      starts_with_only
    )
  )

  ### CHECK IDS ------------------------------------------------------------------

  ids_list <- .timed(
    "Check ids",
    .extract_check_ids(
      data,
      sample,
      text_input,
      id,
      group,
      concepts_list$keys
    )
  )

  data <- ids_list$data
  id <- ids_list$id
  group <- ids_list$group
  which_group <- ids_list$which_group
  nrow_init <- ids_list$nrow_init

  ### ANONYMISATION --------------------------------------------------------------

  # every output frame derives from `data`, so anonymising it once here is what
  # makes the guarantee hold for all of them at the same time
  data <- .timed(
    "Anonymise",
    .extract_anonymise(
      data,
      id,
      group,
      text_input,
      ano_hash,
      ano_hide
    )
  )

  ### FORMAT ---------------------------------------------------------------------

  cli_progress_step("{.strong Formatting source text}")
  br()

  data_token <- .timed(
    "Format text",
    .extract_format_text(
      data,
      text_input,
      id,
      group
    )
  )

  format_drops <- attr(data_token, "format_drops")

  ### TOKENISATION ---------------------------------------------------------------

  cli_progress_step("{.strong Tokenising source text}")
  br()

  # the stages below index by n-gram size rather than by position, so the scalar
  # bound is expanded once here into the named size vector they consume
  token <- set_names(seq_len(ngram_max), paste0("n", seq_len(ngram_max)))

  data_token <- .timed(
    "Tokenise",
    .extract_tokenize(data_token, text_input, token)
  )

  ### MATCHING TOKEN -------------------------------------------------------------

  cli_progress_step("{.strong Matching tokenised text}")
  br()

  match_tokens <- .timed(
    "Match token",
    .extract_match_token(
      data,
      data_token,
      token,
      text_input,
      concepts_list,
      id,
      group,
      intersect
    )
  )

  data_match <- match_tokens$data_match
  data_match_init <- match_tokens$data_match_init
  data_match_init_df <- match_tokens$data_match_init_df
  data_match_df <- match_tokens$data_match_df
  data_token_match <- match_tokens$data_token_match

  ### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusions}")
  br()

  exclusions <- .timed(
    "Exclusions",
    .extract_exclusions(
      data_match,
      text_input,
      id,
      group,
      exclus_manual,
      exclus_auto_escape,
      exclus_auto_token_min
    )
  )

  data_match <- exclusions$data_match
  data_match_exclus <- exclusions$data_match_exclus
  data_match_final <- exclusions$data_match_final
  data_id <- exclusions$data_id
  data_count <- exclusions$data_count
  data_count_exclus <- exclusions$data_count_exclus

  ### MATCHING SOURCE ------------------------------------------------------------

  cli_progress_step("{.strong Matching source text}")
  br()

  match_source <- .timed(
    "Match source",
    .extract_match_source(
      data_match_df,
      data_count,
      text_input,
      id,
      regex_replace
    )
  )

  regex_replace_arg <- match_source$regex_replace_arg
  regex_replace_df <- match_source$regex_replace_df
  data_regex_df <- match_source$data_regex_df
  data_regex_list <- match_source$data_regex_list
  data_regex_match <- match_source$data_regex_match
  data_regex_count <- match_source$data_regex_count
  data_regex_str <- match_source$data_regex_str
  data_regex_prep <- match_source$data_regex_prep

  ### UNMATCHED & MISMATCHED -----------------------------------------------------

  cli_progress_step("{.strong Unmatched documents and source mismatches}")
  br()

  data_unmatched <- .timed(
    "Unmatched",
    .extract_unmatched(
      data,
      data_id,
      data_regex_match,
      id,
      group,
      text_input,
      unmatched_data,
      format_drops
    )
  )

  ### EXTRACTION -----------------------------------------------------------------

  cli_progress_step("{.strong Extraction}")
  br()

  data_extract <- .timed(
    "Extraction",
    .extract_results(
      data_match_df,
      data_id,
      data_regex_match,
      data_regex_str,
      data_regex_prep,
      concepts_list$keys,
      id,
      group
    )
  )

  ### SUMMARY --------------------------------------------------------------------

  cli_progress_step("{.strong Summary}")
  br()

  params <- list(
    sample = sample,
    seed = seed,
    id = id,
    group = if (is.null(which_group)) NULL else group,
    ngram_max = ngram_max,
    concepts_root = concepts_list$root,
    concepts_names = concepts_list$regex_df$concept_name,
    collapse = collapse,
    intersect = intersect,
    starts_with_only = starts_with_only,
    exclus_manual = exclus_manual,
    exclus_auto_escape = exclus_auto_escape,
    exclus_auto_token_min = exclus_auto_token_min,
    regex_replace = regex_replace_arg,
    unmatched_data = unmatched_data,
    concept_color = concept_color,
    text_color = text_color,
    save_as_gt = save_as_gt
  )

  data_summary <- .timed(
    "Summary",
    .extract_summary(
      data_match,
      data_match_exclus,
      data_id,
      data_count,
      id,
      group,
      params
    )
  )

  ### SAVE XLSX ------------------------------------------------------------------

  data_sheets <- .timed(
    "Build sheets",
    .extract_sheets(
      data_extract,
      data_id,
      data_count,
      data_count_exclus,
      data_summary,
      data_unmatched,
      data_regex_df,
      data_regex_match,
      data_regex_count,
      regex_replace_df,
      concepts_list,
      text_input
    )
  )

  data_sheets_df <- map(data_sheets, ~ if (is.data.frame(.x)) .x else .x$data)

  data_sheets_gt <- .timed(
    "Build gt",
    if (save_as_gt) {
      check_installed("gt")

      .extract_sheets_gt(
        data_sheets,
        concepts_list,
        id,
        text_input,
        concept_color,
        text_color
      )
    } else {
      NULL
    }
  )

  cli_progress_step("{.strong {cli_save_extract$xlsx}}")
  br()

  wb <- .timed(
    "Build xlsx",
    .extract_sheets_xlsx(
      data_sheets,
      data_id,
      concepts_list,
      text_input,
      concept_color,
      text_color
    )
  )

  .timed("Save xlsx", wb_save(wb = wb, file = save_extract$xlsx))

  ### SAVE JSON ------------------------------------------------------------------

  cli_progress_step("{.strong {cli_save_extract$json}}")
  br()

  .timed(
    "Save json",
    jsonlite::write_json(
      x = data_summary,
      path = save_extract$json,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  )

  ### SAVE RDS -------------------------------------------------------------------

  cli_progress_step("{.strong {cli_save_extract$rds}}")
  br()

  # a concept leaves every document it did not match untouched, so each pass runs
  # only over its own documents; membership is what `data_regex_match` records
  note_rows <- map(
    set_names(names(data_regex_list)),
    ~ data_extract[[id]] %in%
      data_regex_match[[id]][data_regex_match$concept == .x]
  )

  data_note <- .timed(
    "Note markup",
    data_extract |>
      mutate(
        extract = set_class_css(.data$extract, data_regex_list),
        !!text_input := set_class_css(
          .data[[text_input]],
          data_regex_list,
          rows = note_rows
        )
      ) |>
      select(-any_of(concepts_list$keys))
  )

  data_save <- list(
    data = list(
      base = data |> select(-all_of(text_input)),
      match = data_match_init_df,
      extract = data_extract,
      note = data_note
    ),
    regex = list(
      concepts = concepts_list$regex_df,
      replace = regex_replace_df,
      final = data_regex_df,
      match = data_regex_match
    ),
    match = list(
      init = data_match,
      final = data_match_final
    ),
    count = list(
      init = data_token_match,
      final = data_count
    ),
    exclus = list(
      match = data_match_exclus,
      count = data_count_exclus
    ),
    unmatched = data_unmatched$unmatched,
    mismatched = data_unmatched$mismatched,
    summary = data_summary,
    sheets = list(
      df = data_sheets_df,
      gt = data_sheets_gt
    )
  )

  .timed("Save rds", saveRDS(data_save, file = save_extract$rds))

  ### PRINT ----------------------------------------------------------------------

  cli_progress_done()

  br()
  cli_rule()
  br()

  .extract_print_data <- list(
    token = data_save$count$init,
    count = list(
      total = data_save$count$final,
      token = data_summary$token,
      concepts = data_summary$concept,
      exclus = data_save$exclus$count,
      final = data_save$count$final
    ),
    regex = data_save$regex,
    unmatched = data_save$unmatched,
    mismatched = data_save$mismatched,
    params = data_sheets_df$params
  )

  print(.extract_print_data)

  cli_rule()
  br()

  cli_h2("Timing per step")
  cli_verbatim(unlist(tic.log(format = TRUE)))
  br()

  toc()
  br()

  .extract_print_summary(
    data_save,
    data,
    data_id,
    data_match_init,
    concepts_list,
    nrow_init,
    id,
    group,
    which_group,
    sample,
    intersect,
    unmatched_data,
    save_dir,
    save_files,
    format_drops
  )

  data_save
}

.extract_load <- \(save_dir, save_files, save_extract) {
  cli_h1("edstr_extract")
  br()

  cli_progress_step("Loading file {.strong {save_files}}")
  .load <- readRDS(save_extract$rds)
  cli_progress_done()

  cli_alert_success(
    "File {.strong {save_files}} loaded from {.strong {.path {save_extract$rds}}}"
  )
  br()
  cli_rule()

  .load
}

#' Extract structured variables from clinical text
#'
#' Tokenize source text, match concept patterns (regex), apply exclusions,
#' locate the matched tokens back in the source text, and save results as
#' XLSX, JSON, and RDS files.
#'
#' Requires [edstr_config()] to be called first.
#'
#' @param data `<data.frame>` Input data containing at least a text column
#'   and a unique identifier column.
#' @param text_input `<character(1)>` Name of the text column to analyse.
#'   Defaults to `getOption("edstr_text")` set by [edstr_config()].
#' @param id `<character(1)>` Name of the unique identifier column. If not
#'   supplied, detected automatically: the one column with no duplicates and
#'   no `NA`, aborting if none or several qualify.
#' @param group `<character(1)>` Optional grouping column (e.g. patient ID
#'   when rows are documents). If `NULL`, a sequential `id_group` is created.
#' @param sample `<integer(1)>` Optional. Number of rows to randomly sample
#'   from `data` before extraction.
#' @param seed `<integer(1)>` Optional. Random seed for reproducibility when
#'   `sample` is used.
#' @param ano_hash `<character>` Regex pattern(s) matched case-insensitively
#'   against column names; every matching column is pseudonymised by hashing.
#'   Applied to `data` before extraction, so every output frame (`data$base`,
#'   `data$match`, `data$extract`, `data$note`, `sheets`, and the XLSX and gt
#'   tables built from them) carries the hashed values. The hash is unsalted
#'   and truncated to 16 hexadecimal characters: it is a pseudonym, stable
#'   across runs and therefore reversible by dictionary attack on a small
#'   domain such as a patient identifier. It is not anonymisation.
#' @param ano_hide `<character>` Regex pattern(s) matched case-insensitively
#'   against column names; every matching column is masked (replaced with
#'   `"---"`). Same application point and coverage as `ano_hash`. A column
#'   matched by both arguments is hashed first, then masked, so `"---"` is
#'   what reaches the outputs.
#'
#'   Both arguments abort rather than act silently when a pattern matches no
#'   column at all, matches the `id` or `group` column (which join the outputs
#'   back together), or matches `text_input`. Neither argument touches the
#'   source text: `data$extract`, `data$note` and the highlighted XLSX and gt
#'   output quote `text_input` verbatim by design. Neither touches the Parquet
#'   caches written upstream by [edstr_import()] and [edstr_clean()], which
#'   keep the source in clear.
#' @param ngram_max `<integer(1)>` Largest n-gram size to tokenise. Every
#'   smaller size is searched too, so `ngram_max = 2` covers unigrams and
#'   bigrams. Default `1` (unigrams only). Distinct from the `ngrams` argument
#'   of [edstr_view()], which sets a display window around a match rather than
#'   the sizes searched.
#' @param concepts `<character|list>` Named vector or nested named list of
#'   regex patterns defining the concepts to search for. Each name becomes a
#'   concept key; nested names create sub-concepts (e.g.
#'   `list(cancer = list(sein = "sein|mammaire", poumon = "poumon"))`).
#'   Required.
#' @param collapse `<logical(1)>` If `TRUE`, OR-combine concept patterns into
#'   a single regex: one per root concept as soon as at least one root holds
#'   several patterns, otherwise one regex named `concepts` for the whole set,
#'   which drops the root names. Nesting alone does not decide it: a nested
#'   list whose roots each hold a single pattern takes the second branch.
#'   Requires at least 2 concepts.
#' @param intersect `<logical(1)>` If `TRUE`, keep only documents matching
#'   ALL root-level concepts. Requires at least 2 concepts.
#' @param starts_with_only `<logical(1)>` If `TRUE` (default), token matching
#'   uses prefix mode: the pattern must match the start of a token, and the
#'   rest of the token is accepted (`\\S*$` appended).
#' @param exclus_manual `<character(1)>` Optional regex pattern. Matched
#'   tokens containing this pattern are excluded (manual false-positive
#'   filter).
#' @param exclus_auto_escape `<character(1)>` Optional regex pattern. Tokens
#'   matching this pattern are removed from `data_match` before
#'   auto-exclusion runs.
#' @param exclus_auto_token_min `<numeric(1)>` Minimum n-gram size for
#'   automatic exclusion heuristics (default `10`). Auto-exclusions only
#'   apply to tokens with `n > exclus_auto_token_min`, measured in the same
#'   unit as `ngram_max`. The default therefore disables the heuristic for
#'   every realistic `ngram_max` value: with `ngram_max = 3` no token exceeds
#'   `10`. Set it to `0` to submit every n-gram size to the scan.
#'   When no n-gram clears the threshold the scan has nothing to look at and
#'   is skipped, so the default costs nothing.
#' @param regex_replace `<character>` Optional named vector of additional
#'   regex replacements for source matching (appended to the built-in accent
#'   normalisation rules).
#' @param unmatched_data `<logical(1)>` If `TRUE`, materialise the row-level
#'   `unmatched$no_concept` set (documents whose text was searched and matched
#'   no concept, or no longer match the intersection when `intersect = TRUE`),
#'   which can be large. Only the rows are gated: the count
#'   `unmatched$n_no_concept` is always exact regardless. The `no_source`,
#'   `empty_text` and `outside_p` sets are always populated too. Default
#'   `FALSE`.
#' @param concept_color `<character(1)>` Hex colour for concept highlighting
#'   in XLSX and gt output. Default `"#0099FF"`.
#' @param text_color `<character(1)>` Hex colour for text/extract
#'   highlighting in XLSX and gt output. Default `"#FF0000"`.
#' @param save_as_gt `<logical(1)>` If `TRUE`, generate [gt::gt()] tables
#'   alongside XLSX output. Requires the `gt` package.
#' @param dirname_suffix `<character(1)>` Optional suffix appended to the
#'   `extract/` subdirectory, not to `edstr_dirname` (e.g. `extract_sample_500`).
#'   Defaults to `"sample_{sample}"` when `sample` is set.
#' @param filename_suffix `<character(1)>` Optional suffix appended to output
#'   file names. Defaults to `dirname_suffix`.
#'
#' @return Invisibly, a nested list with elements:
#' \describe{
#'   \item{`data`}{List of data frames: `base` (input without text),
#'     `match` (initial matches), `extract` (final extraction, carrying one
#'     `0`/`1` column per declared concept key, in that order, whether or not
#'     the concept matched anything), `note` (extraction with highlight markup
#'     applied).}
#'   \item{`regex`}{List: `concepts` (parsed patterns), `replace`
#'     (replacement rules), `final` (combined regex), `match` (source-level
#'     matches).}
#'   \item{`match`}{List: `init` (all matches), `final` (keep/drop after
#'     exclusions).}
#'   \item{`count`}{List: `init` (token-level counts), `final` (distinct
#'     match counts).}
#'   \item{`exclus`}{List: `match` (excluded matches), `count` (exclusion
#'     counts).}
#'   \item{`unmatched`}{List for documents with no token match. `n_no_concept`
#'     `<integer(1)>` is the true-negative count, always exact regardless of
#'     `unmatched_data`. Then `id`/`group` tibbles: `no_concept` (text searched,
#'     no concept matched; rows materialised only when `unmatched_data = TRUE`,
#'     otherwise a 0-row tibble while `n_no_concept` still holds the count),
#'     `no_source` (source empty or `NA`, so never searched), `empty_text`
#'     (source holding no text once markup is stripped), `outside_p` (text
#'     outside `<p>` blocks). Read `n_no_concept` for the denominator, never
#'     `nrow(no_concept)`. A document whose every match is excluded counts as a
#'     non-case in `no_concept` too, the exclusions having ruled its matches
#'     false positives, and so does one whose every match fails source
#'     confirmation. Under `intersect = TRUE` the unit is the compound
#'     concept, so a document whose token matches do not cover every root counts
#'     as a non-case as well; the intersection is evaluated on those token
#'     matches, before the exclusions, so a document that loses a whole root to
#'     an exclusion is still delivered, carrying the roots that survived.
#'     `data$match` stays pre-intersect and pre-exclusion, and
#'     `anti_join(data$match, data$extract, by = id)` recovers those sets.}
#'   \item{`mismatched`}{Tibble of token vs source discrepancies (token
#'     matches not confirmed in the source text). One row per match, not per
#'     document, so a document whose matches are only partly confirmed appears
#'     here and in `data$extract` at once.}
#'   \item{`summary`}{List: `token` (summary by token), `concept` (summary
#'     by concept), `params` (call parameters).}
#'   \item{`sheets`}{List: `df` (data frames per Excel sheet), `gt` (gt
#'     tables if `save_as_gt = TRUE`).}
#' }
#' @export
#'
#' @examples
#' \donttest{
#' edstr_config(
#'   edstr_dirname = tempdir(), edstr_filename = "my_study",
#'   edstr_text = "note_text", edstr_overwrite = TRUE
#' )
#'
#' df_clean <- data.frame(
#'   id = 1:3,
#'   note_text = c(
#'     "<p class=\"n\">diabete de type 2</p>",
#'     "<p class=\"n\">bilan normal</p>",
#'     "<p class=\"n\">tumeur mammaire</p>"
#'   )
#' )
#'
#' result <- edstr_extract(
#'   data = df_clean,
#'   concepts = c(diabete = "diabet", cancer = "cancer|tumeur")
#' )
#' }
#'
edstr_extract <- \(
  data,
  text_input = getOption("edstr_text"),
  id = NULL,
  group = NULL,
  sample = NULL,
  seed = NULL,
  ano_hash = NULL,
  ano_hide = NULL,
  ngram_max = 1,
  concepts = NULL,
  collapse = FALSE,
  intersect = FALSE,
  starts_with_only = TRUE,
  exclus_manual = NULL,
  exclus_auto_escape = NULL,
  exclus_auto_token_min = 10,
  regex_replace = NULL,
  unmatched_data = FALSE,
  concept_color = "#0099FF",
  text_color = "#FF0000",
  save_as_gt = FALSE,
  dirname_suffix = if (!is.null(sample)) str_glue("sample_{sample}") else NULL,
  filename_suffix = dirname_suffix
) {
  check_config()

  config_dir <- getOption('edstr_dirname')
  filename <- getOption('edstr_filename')
  dirname <- "extract"

  save_dir <- fs::path(config_dir, dirname)

  if (!is.null(dirname_suffix)) {
    save_dir <- glue("{save_dir}_{dirname_suffix}")
  }

  save_files <- glue("{filename}_{dirname}")

  if (!is.null(filename_suffix)) {
    save_files <- glue("{save_files}_{filename_suffix}")
  }

  save_extract <- map(
    set_names(c("xlsx", "rds", "json")),
    ~ fs::path(save_dir, save_files, ext = .)
  )

  fun_save <- \() {
    .extract_save(
      data,
      text_input,
      id,
      group,
      sample,
      seed,
      ano_hash,
      ano_hide,
      ngram_max,
      concepts,
      collapse,
      intersect,
      starts_with_only,
      exclus_manual,
      exclus_auto_escape,
      exclus_auto_token_min,
      regex_replace,
      unmatched_data,
      concept_color,
      text_color,
      save_as_gt,
      save_dir,
      save_files,
      save_extract
    )
  }

  # the summary is already printed by then, and auto-printing the nested list on
  # top of it would bury it; visibility must not depend on the cache either
  invisible(
    if (fs::file_exists(save_extract$rds)) {
      cli_check(
        config_file = save_files,
        fun_save = fun_save,
        fun_load = \() .extract_load(save_dir, save_files, save_extract)
      )
    } else {
      fun_save()
    }
  )
}
