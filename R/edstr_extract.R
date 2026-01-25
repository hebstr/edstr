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
#' @param text_background text_background
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
  text_background = "#FFFF00",
  dirname_suffix = if (!is.null(sample)) glue("sample_{sample}") else NULL,
  filename_suffix = dirname_suffix,
  load = FALSE
) {

  check_class(data, "data.frame")
  check_class(text_input, "character")

  config <- check_config()

  config_dir <- getOption('edstr_dirname')
  filename <- getOption('edstr_filename')
  dirname <- "extract"

  save_dir <- glue("{config_dir}/{dirname}")

  if (!is.null(dirname_suffix)) save_dir <- glue("{save_dir}_{dirname_suffix}")

  save_files <- glue("{filename}_{dirname}")

  if (!is.null(filename_suffix)) save_files <- glue("{save_files}_{filename_suffix}")

  if (!file.exists(save_dir) && !load) dir.create(path = save_dir)

  save_extract <- glue("{save_dir}/{save_files}")

  save_extract_rds <- glue("{save_extract}.rds")

  if (!is.null(seed)) local_seed(seed)

  tic("Full steps")

  cli_h1("edstr_extract")

  if (!load) {

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

    cli_progress_step("{.strong Formatage du texte source}")
    cli_text("\n\n")

    data_token <- .extract_format_text(
      data, text_input, id, group, ano_hash, ano_hide
    )

### TOKENISATION ---------------------------------------------------------------

    cli_progress_step("{.strong Tokenisation du texte source}")
    cli_text("\n\n")

    token <- set_names(token, paste0("n", token))

    data_token <- .extract_tokenize(data_token, text_input, token)

### MATCHING TOKEN -------------------------------------------------------------

    cli_progress_step("{.strong Matching du texte tokenis\u00e9}")
    cli_text("\n\n")

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

    cli_progress_step("{.strong Exclusions}")
    cli_text("\n\n")

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

    cli_progress_step("{.strong Matching du texte source}")
    cli_text("\n\n")

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

    cli_progress_step("{.strong Mismatch texte source / texte tokenis\u00e9}")
    cli_text("\n\n")

    data_mismatch <- .extract_mismatch(
      data, data_match, data_match_init, data_regex_match, id, group, text_input, mismatch_data
    )

### EXTRACTION -----------------------------------------------------------------

    cli_progress_step("{.strong Extraction}")
    cli_text("\n\n")

    data_extract <- .extract_results(
      data_match_df, data_id, data_regex_list, data_regex_str, concepts_list$root, id, group, text_input
    )

### RESUME ---------------------------------------------------------------------

    cli_progress_step("{.strong R\u00e9sum\u00e9}")
    cli_text("\n\n")

    params <- list(
      sample = sample,
      seed = seed,
      id = id,
      group = group,
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
      text_color = text_color,
      text_background = text_background
    )

    data_summary <- .extract_summary(
      data_match, data_match_exclus, data_id, data_count, id, group, params
    )

    unlink(file.path(save_dir, list.files(save_dir)), recursive = TRUE)

### XLSX DATA ------------------------------------------------------------------

  cli_progress_step("{.strong Enregistrement du .xlsx}")
  cli_text("\n\n")

  .sheet_mismatch <-
  lst(
    data = data_mismatch$regex,
    rows = if (nrow(data) > 0) data else add_row(data),
    visible = if (nrow(data) > 0) "true" else "false"
  )

  .sheet_params <-
  data_summary$params |>
    map(as.character) |>
    enframe(name = "arg") |>
    mutate(
      value = map_chr(value, ~ str_flatten(unlist(.), " ; ")),
      value = ifelse(value == "", "NULL", value)
    )

  data_sheets <-
  list(
    data = data_extract |> select(-text_input),
    token_regex = concepts_list$regex_df,
    token_match = data_id |> select(-concept_key),
    token_count = data_count |> select(-token),
    concept_count = data_summary$concept,
    text_replace = regex_replace_df,
    text_regex = data_regex_df,
    text_match = data_regex_match,
    text_count = data_regex_count,
    mismatch = .sheet_mismatch$data,
    params = .sheet_params
  ) |>
    imap(~ list2(name = .y, data = .x))

  .wb_text_var <- text_input
  .wb_concept_var <- "concept"

  xlsx_output <-
  wb_workbook() |>
    wb_add_custom(
      sheet = data_sheets$data$name,
      data = data_sheets$data$data,
      concept_var = c(data_id$concept_key, "concept"),
      concept_color = concept_color,
      text_var = "extract",
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$token_regex$name,
      data = data_sheets$token_regex$data,
      concept_var = names(concepts_list$regex_df) |> str_subset("concept"),
      concept_color = concept_color,
      text_var = "regex",
      text_color = text_color,
      halign = "left"
    ) |>
    wb_add_custom(
      sheet = data_sheets$token_match$name,
      data = data_sheets$token_match$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = .wb_text_var,
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$token_count$name,
      data = data_sheets$token_count$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = .wb_text_var,
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$concept_count$name,
      data = data_sheets$concept_count$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_replace$name,
      data = data_sheets$text_replace$data,
      halign = "left"
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_regex$name,
      data = data_sheets$text_regex$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = .wb_text_var,
      text_color = text_color,
      halign = "left",
      max_width = 150
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_match$name,
      data = data_sheets$text_match$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = "match",
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_count$name,
      data = data_sheets$text_count$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = "match",
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$mismatch$name,
      data = .sheet_mismatch$rows,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = "match",
      text_color = text_color,
      visible = .sheet_mismatch$visible
    ) |>
    wb_add_custom(
      sheet = data_sheets$params$name,
      data = data_sheets$params$data,
      halign = "left"
    )

  data_xlsx_tbl <- data_sheets |> list_transpose() |> pluck("data")

  xlsx_output |> wb_save(file = glue("{save_extract}.xlsx"))

  data_xlsx_gt <-
  list2("{data_sheets$data$name}" :=
          list(
            id =
              data_sheets$data$data |>
                select(-c(matches(concepts_list$root), concept, extract)) |>
                gt_custom(font_size = 11) |>
                sub_missing() |>
                gt_text_align(),
            text =
              data_sheets$data$data |>
                select(n, id, matches(concepts_list$root), concept, extract) |>
                mutate(` ... ` = "...", .after = id) |>
                gt_custom(font_size = 11) |>
                gt_text_align() |>
                gt_text_color(column = matches(c(concepts_list$root, "concept")),
                              color = concept_color) |>
                gt_text_color(column = "extract",
                              color = text_color)
          ),
        "{data_sheets$token_regex$name}" :=
          data_sheets$token_regex$data |>
            gt_custom(head = NULL) |>
            gt_text_color(column = matches("concept"),
                          color = concept_color) |>
            gt_text_color(column = "regex",
                          color = text_color) |>
            gt_code_font(column = "regex"),
        "{data_sheets$token_match$name}" :=
          data_sheets$token_match$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = text_input,
                          color = text_color),
        "{data_sheets$token_count$name}" :=
          data_sheets$token_count$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = text_input,
                          color = text_color),
        "{data_sheets$concept_count$name}" :=
          data_sheets$concept_count$data |>
            gt_custom() |>
            sub_missing() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color),
        "{data_sheets$text_replace$name}" :=
          data_sheets$text_replace$data |>
            gt_custom(head = NULL) |>
            gt_code_font(),
        "{data_sheets$text_regex$name}" :=
          data_sheets$text_regex$data |>
            gt_custom(head = NULL) |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = text_input,
                          color = text_color) |>
            gt_code_font(column = text_input),
        "{data_sheets$text_match$name}" :=
          data_sheets$text_match$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = "match",
                          color = text_color),
        "{data_sheets$text_count$name}" :=
          data_sheets$text_count$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = "match",
                          color = text_color),
        "{data_sheets$mismatch$name}" :=
          data_sheets$mismatch$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = "match",
                          color = text_color) |>
            sub_missing(),
        "{data_sheets$params$name}" :=
          data_sheets$params$data |>
            gt_custom(head = NULL))

### CSV DATA -------------------------------------------------------------------

  cli_progress_step("{.strong Enregistrement du .csv}")
  cli_text("\n\n")

  data_csv <-
  data_extract |>
    set_data_csv(
      text_input = text_input,
      text_color = text_color,
      text_background = text_background,
      pattern = data_regex_str
    ) |>
    select(-matches(concepts_list$root))

  write_excel_csv(data_csv, file = glue("{save_extract}.csv"))

### ASSIGN DATA ----------------------------------------------------------------

  cli_progress_step("{.strong Enregistrement du .rds}")
  cli_text("\n\n")

  data_save <- list(
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
    data = list(
      base = data |> select(-text_input),
      match = data_match_init_df,
      extract = data_extract,
      xlsx = list(
        tbl = data_xlsx_tbl,
        gt = data_xlsx_gt
      ),
      csv = data_csv
    )
  )

  readr::write_rds(
    x = data_save,
    file = save_extract_rds
  )

  ### PRINT ----------------------------------------------------------------------

  cli_progress_done()

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  data_print <- list(
    token = data_token_match,
    count = list(
      total = data_save$count$final,
      token = data_summary$token,
      concepts = data_summary$concept,
      exclus = data_save$exclus$count,
      final = data_save$count$final
    ),
    regex = data_save$regex,
    mismatch = data_save$mismatch,
    params = data_summary$params
  )

  print(data_print)

  cli_rule()
  cli_text("\n\n")

### CLI ------------------------------------------------------------------------

  toc()
  cli_text("\n\n")

  n_concepts <- length(concepts_list$root)

  cli_intersect <- if (intersect) " \u00e0 l'intersection {concepts_list$str$inter}" else ""

  cli_n_id <- nrow(data_id |> filter(.data[[id]] %in% match_id[[id]]))
  cli_p_id <- label_percent(0.1)(nrow(data) / nrow_init)

  cli_n_match <- n_distinct(data_match_init[[id]])
  cli_p_match <- label_percent(0.1)(cli_n_match / nrow(data))

  cli_n_extract <- nrow(data_extract)
  cli_p_extract <- label_percent(0.1)(cli_n_extract / nrow(data))

  cli_n_group <- n_distinct(data_extract[[group]])
  cli_p_group <- label_percent(0.1)(cli_n_group / n_distinct(data[[group]]))

  cli_n_mismatch <- nrow(data_mismatch$id)
  cli_p_mismatch <- label_percent(0.1)(cli_n_mismatch / nrow(data))

  cli_n_group_mismatch <- n_distinct(data_mismatch$id[[group]])
  cli_p_group_mismatch <- label_percent(0.1)(cli_n_group_mismatch / n_distinct(data[[group]]))

  cli_n_files <- sum(str_detect(list.files(save_dir), "\\.\\w+"))

  cli_col <- \(x) col_blue(glue(x))

  cli_alert_info("{.strong Documents}")
  cli_ul()
  cli_ul()
    cli_li("Total : {nrow_init} {id}")
    if (!is.null(sample)) cli_li("Sample : {nrow(data)} {id} ({cli_p_id})")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Correspondances totales}")
  cli_ul()
    cli_li("{nrow(data_match_init)} parmi {cli_n_match} {id} ({cli_p_match} {id})")
    if (nrow(data_count_exclus) == 0) cli_li("Aucune exclusion")
    cli_end()

  if (nrow(data_count_exclus) > 0) {

    cli_text("\n\n")
    cli_alert_info("{.strong Exclusions}")
    cli_ul()
      cli_li("Automatiques : {nrow(data_match_exclus$auto)}")
      cli_li("Manuelles : {nrow(data_match_exclus$manual)}")
      cli_end()

  }

  if (mismatch_data) {

    cli_text("\n\n")
    cli_alert_info("{.strong Mismatch :} {cli_n_mismatch} {id}")

  }

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {n_concepts} concept{?s} parent{?s} :} {concepts_list$str$comma}")
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_id} correspondance{?s}{glue(cli_intersect)}}")
  cli_ul()
    cli_li("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (!is.null(which_group)) cli_li("{cli_n_group} {group} ({cli_p_group} {group})")
    cli_end()

  if (mismatch_data && cli_n_mismatch > 0) {

    cli_text("\n\n")
    cli_alert_success("{.strong {cli_n_mismatch} mismatch{?es}}")
    cli_ul()
      cli_li("{cli_n_mismatch} {id} ({cli_p_mismatch} {id})")
      if (!is.null(which_group)) cli_li("{cli_n_group_mismatch} {group} ({cli_p_group_mismatch} {group})")
      cli_end()

  }

  cli_text("\n\n")
  cli_alert_success("{.strong {nrow(data_count)} correspondance{?s} distincte{?s}}")

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_files} fichier{?s} enregistr\u00e9{?s} dans le r\u00e9pertoire {.path {save_dir}}}")
  cli_ul()
    cli_li("rds : {cli_col('{save_files}.rds')}")
    cli_li("xlsx : {cli_col('{save_files}.xlsx')}")
    cli_li("csv : {cli_col('{save_files}.csv')}")
    cli_end()

  cli_text("\n\n")
  cli_rule()

  return(invisible(data_save))

  } else {

    cli_load(
      dir = save_dir,
      file = save_files,
      save = save_extract_rds
    )

  }

}
