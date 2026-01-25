.extract_prep_sheets <- \(
  data_extract,
  data_id,
  data_count,
  data_summary,
  data_mismatch,
  data_regex_df,
  data_regex_match,
  data_regex_count,
  regex_replace_df,
  concepts_list,
  text_input
) {

  .sheet_mismatch <- lst(
    data = data_mismatch$regex,
    rows = if (nrow(data_mismatch$regex) > 0) data else add_row(data_mismatch$regex),
    visible = if (nrow(data_mismatch$regex) > 0) "true" else "false"
  )

  .sheet_params <-
  data_summary$params |>
    map(as.character) |>
    enframe(name = "arg") |>
    mutate(
      value = map_chr(value, ~ str_flatten(unlist(.), " ; ")),
      value = ifelse(value == "", "NULL", value)
    )

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
    mismatch = .sheet_mismatch,
    params = .sheet_params
  ) |>
    imap(~ list2(name = .y, data = .x))

}

.extract_export_xlsx <- \(
  data_sheets,
  save_extract,
  data_id,
  concepts_list,
  text_input,
  concept_color,
  text_color
) {

  cli_progress_step("{.strong Enregistrement du .xlsx}")
  cli_text("\n\n")

  .cv <- "concept"
  .tv <- text_input

  sheet_configs <- list(
    data = list(
      cv = c(data_id$concept_key, "concept"),
      tv = "extract"
    ),
    token_regex = list(
      cv = names(concepts_list$regex_df) |> str_subset("concept"),
      tv = "regex",
      halign = "left"
    ),
    token_match = list(cv = .cv, tv = .tv),
    token_count = list(cv = .cv, tv = .tv),
    concept_count = list(cv = .cv, tv = NULL),
    text_replace = list(cv = NULL, tv = NULL, halign = "left"),
    text_regex = list(cv = .cv, tv = .tv, halign = "left", max_width = 150),
    text_match = list(cv = .cv, tv = "match"),
    text_count = list(cv = .cv, tv = "match"),
    mismatch = list(
      cv = .cv,
      tv = "match",
      visible = data_sheets$mismatch$data$visible
    ),
    params = list(
      cv = NULL,
      tv = NULL,
      halign = "left"
    )
  )

  xlsx_output <- purrr::reduce2(
    names(sheet_configs),
    sheet_configs,
    \(wb, name, cfg) {
      sheet_data <- if (name == "mismatch") {
        data_sheets$mismatch$data$rows
      } else {
        data_sheets[[name]]$data
      }
      wb_add_custom(
        wb,
        sheet = name,
        data = sheet_data,
        concept_var = cfg$cv,
        concept_color = if (!is.null(cfg$cv)) concept_color else NULL,
        text_var = cfg$tv,
        text_color = if (!is.null(cfg$tv)) text_color else NULL,
        halign = cfg$halign %||% NULL,
        max_width = cfg$max_width %||% NULL,
        visible = cfg$visible %||% NULL
      )
    },
    .init = wb_workbook()
  )

  wb_save(xlsx_output, file = glue("{save_extract}.xlsx"))

  data_xlsx_tbl <- data_sheets |> list_transpose() |> pluck("data")

  lst(
    xlsx_output = xlsx_output,
    data_xlsx_tbl = data_xlsx_tbl
  )

}
