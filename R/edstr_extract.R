.extract_save <- \(
  data,
  text_input,
  id,
  group,
  sample,
  seed,
  ano_hash,
  ano_hide,
  token,
  concepts,
  collapse,
  intersect,
  starts_with_only,
  exclus_manual,
  exclus_auto_escape,
  exclus_auto_token_min,
  regex_replace,
  mismatch_data,
  concept_color,
  text_color,
  save_as_gt,
  save_dir,
  save_files,
  save_extract
) {
  check_class(data, "data.frame")
  check_class(text_input, "character")

  fs::dir_create(save_dir)

  cli_save_extract <- map(
    set_names(c("xlsx", "rds", "csv")),
    ~ paste("Saving file", fs::path(save_files, ext = .))
  )

  if (!is.null(seed)) {
    local_seed(seed)
  }

  tic("Full steps")

  cli_h1("edstr_extract")

  ### PARSE CONCEPTS -------------------------------------------------------------

  concepts_list <- .extract_parse_concepts(
    concepts,
    collapse,
    intersect,
    starts_with_only
  )

  ### CHECK IDS ------------------------------------------------------------------

  ids_list <- .extract_check_ids(data, sample, text_input, id, group)

  data <- ids_list$data
  id <- ids_list$id
  group <- ids_list$group
  which_group <- ids_list$which_group
  nrow_init <- ids_list$nrow_init

  ### FORMAT ---------------------------------------------------------------------

  cli_progress_step("{.strong Formatting source text}")
  br()

  data_token <- .extract_format_text(
    data,
    text_input,
    id,
    group,
    ano_hash,
    ano_hide
  )

  ### TOKENISATION ---------------------------------------------------------------

  cli_progress_step("{.strong Tokenising source text}")
  br()

  token <- set_names(token, paste0("n", token))

  data_token <- .extract_tokenize(data_token, text_input, token)

  ### MATCHING TOKEN -------------------------------------------------------------

  cli_progress_step("{.strong Matching tokenised text}")
  br()

  match_tokens <- .extract_match_token(
    data,
    data_token,
    token,
    text_input,
    concepts_list,
    id,
    group,
    intersect
  )

  data_match <- match_tokens$data_match
  data_match_init <- match_tokens$data_match_init
  data_match_init_df <- match_tokens$data_match_init_df
  data_match_df <- match_tokens$data_match_df
  data_token_match <- match_tokens$data_token_match
  match_id <- match_tokens$match_id

  ### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusions}")
  br()

  exclusions <- .extract_exclusions(
    data_match,
    text_input,
    id,
    group,
    exclus_manual,
    exclus_auto_escape,
    exclus_auto_token_min
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

  match_source <- .extract_match_source(
    data_match_df,
    data_count,
    text_input,
    id,
    regex_replace
  )

  regex_replace_arg <- match_source$regex_replace_arg
  regex_replace_df <- match_source$regex_replace_df
  data_regex_df <- match_source$data_regex_df
  data_regex_list <- match_source$data_regex_list
  data_regex_match <- match_source$data_regex_match
  data_regex_count <- match_source$data_regex_count
  data_regex_str <- match_source$data_regex_str

  ### MISMATCH -------------------------------------------------------------------

  cli_progress_step("{.strong Mismatch between source and tokenised text}")
  br()

  data_mismatch <- .extract_mismatch(
    data,
    data_match,
    data_match_init,
    data_regex_match,
    id,
    group,
    text_input,
    mismatch_data
  )

  ### EXTRACTION -----------------------------------------------------------------

  cli_progress_step("{.strong Extraction}")
  br()

  data_extract <- .extract_results(
    data_match_df,
    data_id,
    data_regex_list,
    data_regex_str,
    concepts_list$root,
    id,
    group,
    text_input
  )

  ### SUMMARY --------------------------------------------------------------------

  cli_progress_step("{.strong Summary}")
  br()

  params <- list(
    sample = sample,
    seed = seed,
    id = id,
    group = if (is.null(which_group)) NULL else group,
    token = token,
    concepts_root = concepts_list$root,
    concepts_names = concepts_list$regex_df$concept_name,
    collapse = collapse,
    intersect = intersect,
    starts_with_only = starts_with_only,
    exclus_manual = exclus_manual,
    exclus_auto_escape = exclus_auto_escape,
    exclus_auto_token_min = exclus_auto_token_min,
    regex_replace = regex_replace_arg,
    mismatch_data = mismatch_data,
    concept_color = concept_color,
    text_color = text_color,
    save_as_gt = save_as_gt
  )

  data_summary <- .extract_summary(
    data_match,
    data_match_exclus,
    data_id,
    data_count,
    id,
    group,
    params
  )

  ### SAVE XLSX ------------------------------------------------------------------

  data_sheets <- .extract_sheets(
    data_extract,
    data_id,
    data_count,
    data_count_exclus,
    data_summary,
    data_mismatch,
    data_regex_df,
    data_regex_match,
    data_regex_count,
    regex_replace_df,
    concepts_list,
    text_input
  )

  data_sheets_df <- map(data_sheets, ~ if (is.data.frame(.x)) .x else .x$data)

  data_sheets_gt <- if (save_as_gt) {
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

  cli_progress_step("{.strong {cli_save_extract$xlsx}}")
  br()

  wb_save(
    wb = .extract_sheets_xlsx(
      data_sheets,
      data_id,
      concepts_list,
      text_input,
      concept_color,
      text_color
    ),
    file = save_extract$xlsx
  )

  ### SAVE CSV -------------------------------------------------------------------

  cli_progress_step("{.strong {cli_save_extract$csv}}")
  br()

  data_csv <-
    data_extract |>
    mutate(
      across(
        all_of(c("extract", text_input)),
        ~ set_class_css(., data_regex_list)
      ),
      extract = str_remove_all(.data$extract, ";")
    ) |>
    select(-matches(concepts_list$root))

  write_excel_csv(data_csv, file = save_extract$csv)

  ### SAVE RDS -------------------------------------------------------------------

  cli_progress_step("{.strong {cli_save_extract$rds}}")
  br()

  data_save <- list(
    data = list(
      base = data |> select(-all_of(text_input)),
      match = data_match_init_df,
      extract = data_extract,
      csv = data_csv
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
    mismatch = data_mismatch,
    summary = data_summary,
    sheets = list(
      df = data_sheets_df,
      gt = data_sheets_gt
    )
  )

  saveRDS(data_save, file = save_extract$rds)

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
    mismatch = data_save$mismatch,
    params = data_sheets_df$params
  )

  print(.extract_print_data)

  cli_rule()
  br()

  toc()
  br()

  .extract_print_summary(
    data_save,
    data,
    data_id,
    data_match_init,
    match_id,
    concepts_list,
    nrow_init,
    id,
    group,
    which_group,
    sample,
    intersect,
    mismatch_data,
    save_dir,
    save_files
  )

  data_save
}

.extract_load <- \(save_dir, save_files, save_extract) {
  cli_h1("edstr_extract")
  br()

  cli_load(
    dir = save_dir,
    file = save_files,
    save = save_extract$rds
  )
}

#' Extract structured variables from clinical text
#'
#' Tokenize source text, match concept patterns (regex), apply exclusions,
#' perform source-level re-matching with accent normalisation, and save
#' results as XLSX, CSV, and RDS files.
#'
#' Requires [edstr_config()] to be called first.
#'
#' @param data `<data.frame>` Input data containing at least a text column
#'   and a unique identifier column.
#' @param text_input `<character(1)>` Name of the text column to analyse.
#'   Defaults to `getOption("edstr_text")` set by [edstr_config()].
#' @param id `<character(1)>` Name of the unique identifier column. If not
#'   supplied, automatically detected (first column with no duplicates and
#'   no `NA`).
#' @param group `<character(1)>` Optional grouping column (e.g. patient ID
#'   when rows are documents). If `NULL`, a sequential `id_group` is created.
#' @param sample `<integer(1)>` Optional. Number of rows to randomly sample
#'   from `data` before extraction.
#' @param seed `<integer(1)>` Optional. Random seed for reproducibility when
#'   `sample` is used.
#' @param ano_hash `<character>` Column name(s) to pseudonymise by hashing.
#' @param ano_hide `<character>` Column name(s) to pseudonymise by masking
#'   (replaced with `"---"`).
#' @param token `<integer>` N-gram sizes to use for tokenisation. Default `1`
#'   (unigrams). Use `c(1, 2)` for unigrams and bigrams.
#' @param concepts `<character|list>` Named vector or nested named list of
#'   regex patterns defining the concepts to search for. Each name becomes a
#'   concept key; nested names create sub-concepts (e.g.
#'   `list(cancer = c(sein = "sein|mammaire", poumon = "poumon"))`.
#' @param collapse `<logical(1)>` If `TRUE`, OR-collapse all concept patterns
#'   into a single regex per root concept. Requires at least 2 concepts.
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
#'   apply to tokens with `n > exclus_auto_token_min`.
#' @param regex_replace `<character>` Optional named vector of additional
#'   regex replacements for source matching (appended to the built-in accent
#'   normalisation rules).
#' @param mismatch_data `<logical(1)>` If `TRUE`, include unmatched documents
#'   in the mismatch output. Default `FALSE`.
#' @param concept_color `<character(1)>` Hex colour for concept highlighting
#'   in XLSX and gt output. Default `"#0099FF"`.
#' @param text_color `<character(1)>` Hex colour for text/extract
#'   highlighting in XLSX and gt output. Default `"#FF0000"`.
#' @param save_as_gt `<logical(1)>` If `TRUE`, generate [gt::gt()] tables
#'   alongside XLSX output. Requires the `gt` package.
#' @param dirname_suffix `<character(1)>` Optional suffix appended to the
#'   output directory name. Defaults to `"sample_{sample}"` when `sample` is
#'   set.
#' @param filename_suffix `<character(1)>` Optional suffix appended to output
#'   file names. Defaults to `dirname_suffix`.
#'
#' @return A nested list (invisibly returned from cache when the RDS file
#'   already exists) with elements:
#' \describe{
#'   \item{`data`}{List of data frames: `base` (input without text),
#'     `match` (initial matches), `extract` (final extraction), `csv`
#'     (CSV-ready output).}
#'   \item{`regex`}{List: `concepts` (parsed patterns), `replace`
#'     (replacement rules), `final` (combined regex), `match` (source-level
#'     matches).}
#'   \item{`match`}{List: `init` (all matches), `final` (keep/drop after
#'     exclusions).}
#'   \item{`count`}{List: `init` (token-level counts), `final` (distinct
#'     match counts).}
#'   \item{`exclus`}{List: `match` (excluded matches), `count` (exclusion
#'     counts).}
#'   \item{`mismatch`}{List: `id` (unmatched IDs), `regex` (token vs source
#'     discrepancies).}
#'   \item{`summary`}{List: `token` (summary by token), `concept` (summary
#'     by concept), `params` (call parameters).}
#'   \item{`sheets`}{List: `df` (data frames per Excel sheet), `gt` (gt
#'     tables if `save_as_gt = TRUE`).}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' edstr_config(edstr_dirname = "output", edstr_filename = "my_study")
#'
#' df <- edstr_import(query = "sql/my_query.sql")
#'
#' result <- edstr_extract(
#'   data = df,
#'   concepts = c(diabete = "diabet", cancer = "cancer|tumeur"),
#'   token = c(1, 2),
#'   intersect = TRUE
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
  token = 1,
  concepts,
  collapse = FALSE,
  intersect = FALSE,
  starts_with_only = TRUE,
  exclus_manual = NULL,
  exclus_auto_escape = NULL,
  exclus_auto_token_min = 10,
  regex_replace = NULL,
  mismatch_data = FALSE,
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
    set_names(c("xlsx", "rds", "csv")),
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
      token,
      concepts,
      collapse,
      intersect,
      starts_with_only,
      exclus_manual,
      exclus_auto_escape,
      exclus_auto_token_min,
      regex_replace,
      mismatch_data,
      concept_color,
      text_color,
      save_as_gt,
      save_dir,
      save_files,
      save_extract
    )
  }

  if (fs::file_exists(save_extract$rds)) {
    cli_check(
      config_file = save_files,
      fun_save = fun_save,
      fun_load = \() .extract_load(save_dir, save_files, save_extract)
    )
  } else {
    fun_save()
  }
}
