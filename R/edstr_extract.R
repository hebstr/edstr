#' Title
#'
#' @param data data
#' @param text_input text_input
#' @param sample sample
#' @param seed seed
#' @param ano_hash ano_hash
#' @param ano_hide ano_hide
#' @param id id
#' @param group group
#' @param token token
#' @param concepts concepts
#' @param collapse collapse
#' @param intersect intersect
#' @param starts_with_only starts_with_only
#' @param exclus_manual exclus_manual
#' @param exclus_auto_escape exclus_auto_escape
#' @param regex_replace regex_replace
#' @param mismatch_data mismatch_data
#' @param concept_color concept_color
#' @param text_color text_color
#' @param dirname_suffix dirname_suffix
#' @param filename_suffix filename_suffix
#' @param load load
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_extract <- \(
  data,
  text_input = getOption("edstr_text"),
  id = check_id_key(data = data, exclude = text_input),
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
  regex_replace = NULL,
  mismatch_data = FALSE,
  concept_color = "#0099FF",
  text_color = "#FF0000",
  dirname_suffix = if (!is.null(sample)) str_glue("sample_{sample}") else NULL,
  filename_suffix = dirname_suffix,
  load = FALSE
) {

  check_class(data, "data.frame")
  check_class(text_input, "character")

  config <- check_config()

  config_dir <- getOption('edstr_dirname')
  filename <- getOption('edstr_filename')
  dirname <- "extract"

  save_dir <- fs::path(config_dir, dirname)

  if (!is.null(dirname_suffix)) save_dir <- glue("{save_dir}_{dirname_suffix}")

  save_files <- glue("{filename}_{dirname}")

  if (!is.null(filename_suffix)) save_files <- glue("{save_files}_{filename_suffix}")

  cli_save_file <- \(x) paste(
    "Enregistrement du fichier", fs::path(save_files, ext = x)
  )

  if (!load) fs::dir_create(save_dir)

  save_extract <- fs::path(save_dir, save_files)

  save_extract_rds <- glue("{save_extract}.rds")

  if (!is.null(seed)) local_seed(seed)

  tic("Full steps")

  cli_h1("edstr_extract")

  if (!load) {

### PARSE CONCEPTS -------------------------------------------------------------

    concepts_list <- .extract_parse_concepts(
      concepts, collapse, intersect, starts_with_only
    )

### CHECK IDS ------------------------------------------------------------------

    ids_list <- .extract_check_ids(data, sample, text_input, id, group)

    data <- ids_list$data
    id <- ids_list$id
    group <- ids_list$group
    which_group <- ids_list$which_group
    nrow_init <- ids_list$nrow_init

### FORMAT ---------------------------------------------------------------------

    cli_progress_step("{.strong Formatage du texte source}"); br()

    data_token <- .extract_format_text(
      data, text_input, id, group, ano_hash, ano_hide
    )

### TOKENISATION ---------------------------------------------------------------

    cli_progress_step("{.strong Tokenisation du texte source}"); br()

    token <- set_names(token, paste0("n", token))

    data_token <- .extract_tokenize(data_token, text_input, token)

### MATCHING TOKEN -------------------------------------------------------------

    cli_progress_step("{.strong Matching du texte tokenis\u00e9}"); br()

    match_tokens <- .extract_match_token(
      data, data_token, token, text_input, concepts_list, id, group, intersect
    )

    data_match <- match_tokens$data_match
    data_match_init <- match_tokens$data_match_init
    data_match_init_df <- match_tokens$data_match_init_df
    data_match_df <- match_tokens$data_match_df
    data_token_match <- match_tokens$data_token_match
    match_id <- match_tokens$match_id

### EXCLUSIONS -----------------------------------------------------------------

    cli_progress_step("{.strong Exclusions}"); br()

    exclusions <- .extract_exclusions(
      data_match, text_input, id, group, exclus_manual, exclus_auto_escape
    )

    data_match <- exclusions$data_match
    data_match_exclus <- exclusions$data_match_exclus
    data_match_final <- exclusions$data_match_final
    data_id <- exclusions$data_id
    data_count <- exclusions$data_count
    data_count_exclus <- exclusions$data_count_exclus

### MATCHING SOURCE ------------------------------------------------------------

    cli_progress_step("{.strong Matching du texte source}"); br()

    match_source <- .extract_match_source(
      data_match_df, data_count, text_input, id, regex_replace
    )

    regex_replace_arg <- match_source$regex_replace_arg
    regex_replace_df <- match_source$regex_replace_df
    data_regex_df <- match_source$data_regex_df
    data_regex_list <- match_source$data_regex_list
    data_regex_match <- match_source$data_regex_match
    data_regex_count <- match_source$data_regex_count
    data_regex_str <- match_source$data_regex_str

### MISMATCH -------------------------------------------------------------------

    cli_progress_step("{.strong Mismatch entre texte source et texte tokenis\u00e9}"); br()

    data_mismatch <- .extract_mismatch(
      data, data_match, data_match_init, data_regex_match,
      id, group, text_input, mismatch_data
    )

### EXTRACTION -----------------------------------------------------------------

    cli_progress_step("{.strong Extraction}"); br()

    data_extract <- .extract_results(
      data_match_df, data_id, data_regex_list, data_regex_str,
      concepts_list$root, id, group, text_input
    )

### RESUME ---------------------------------------------------------------------

    cli_progress_step("{.strong R\u00e9sum\u00e9}"); br()

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
      regex_replace = regex_replace_arg,
      mismatch_data = mismatch_data,
      concept_color = concept_color,
      text_color = text_color
    )

    data_summary <- .extract_summary(
      data_match, data_match_exclus, data_id, data_count, id, group, params
    )

    fs::file_delete(fs::dir_ls(save_dir))

### SAVE XLSX ------------------------------------------------------------------

    data_sheets <- .extract_sheets(
      data_extract, data_id, data_count, data_count_exclus, data_summary,
      data_mismatch, data_regex_df, data_regex_match, data_regex_count,
      regex_replace_df, concepts_list, text_input
    )

    data_sheets_df <- map(data_sheets, ~ if (is.data.frame(.x)) .x else .x$data)

    data_sheets_gt <- .extract_sheets_gt(
      data_sheets, concepts_list, id, text_input, concept_color, text_color
    )

    cli_progress_step("{.strong {cli_save_file('xlsx')}}"); br()

    wb_save(
      wb = .extract_sheets_xlsx(
        data_sheets, data_id, concepts_list,
        text_input, concept_color, text_color
      ),
      file = glue("{save_extract}.xlsx")
    )

### SAVE CSV -------------------------------------------------------------------

    cli_progress_step("{.strong {cli_save_file('csv')}}"); br()

    data_csv <-
    data_extract |>
      mutate(
        across(c(extract, text_input), ~ set_class_css(., data_regex_list)),
        extract = str_remove_all(extract, ";")
      ) |>
      select(-matches(concepts_list$root))

    write_excel_csv(data_csv, file = glue("{save_extract}.csv"))

### SAVE RDS -------------------------------------------------------------------

    cli_progress_step("{.strong {cli_save_file('rds')}}"); br()

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

    saveRDS(data_save, file = save_extract_rds)

### PRINT ----------------------------------------------------------------------

    cli_progress_done()

    br(); cli_rule(); br()

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

    cli_rule(); br()

    toc(); br()

    .extract_print_summary(
      data_save, data, data_id, data_match_init, match_id,
      concepts_list, nrow_init, id, group, which_group,
      sample, intersect, mismatch_data, save_dir, save_files
    )

    return(invisible(data_save))

### LOAD -----------------------------------------------------------------------

  } else {

    cli_load(
      dir = save_dir,
      file = save_files,
      save = save_extract_rds
    )

  }

}
