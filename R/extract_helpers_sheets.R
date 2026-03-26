.extract_sheets <- \(
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
) {

  .mismatch_data <- data_mismatch$regex

  .sheet_exclusions <- lst(
    data = data_count_exclus,
    rows = if (nrow(data_count_exclus) > 0) data_count_exclus else add_row(data_count_exclus),
    visible = if (nrow(data_count_exclus) > 0) "true" else "false"
  )

  .sheet_mismatch <- lst(
    data = .mismatch_data,
    rows = if (nrow(.mismatch_data) > 0) .mismatch_data else add_row(.mismatch_data),
    visible = if (nrow(.mismatch_data) > 0) "true" else "false"
  )

  .sheet_params <-
  data_summary$params |>
    map(as.character) |>
    enframe(name = "arg") |>
    mutate(
      value = map_chr(.data$value, ~ str_flatten(unlist(.), " ; ")),
      value = ifelse(.data$value == "", "NULL", .data$value)
    )

  list(
    data = data_extract |> select(-all_of(text_input)),
    token_regex = concepts_list$regex_df,
    token_match = data_id |> select(-"concept_key"),
    token_count = data_count |> select(-"token"),
    token_exclusion = .sheet_exclusions,
    concept_count = data_summary$concept,
    text_replace = regex_replace_df,
    text_regex = data_regex_df,
    text_match = data_regex_match,
    text_count = data_regex_count,
    mismatch = .sheet_mismatch,
    params = .sheet_params
  )

}

.extract_sheets_xlsx <- \(
  data_sheets,
  data_id,
  concepts_list,
  text_input,
  concept_color,
  text_color
) {

  .cv <- "concept"
  .tv <- text_input

  mk_color <- \(cv = NULL, tv = NULL) {

    list(cv, tv) |>
      set_names(c(concept_color, text_color)) |>
      compact()

  }

  sheet_configs <- list(
    data = list(color = mk_color(c(data_id$concept_key, "concept"), "extract")),
    token_regex = list(
      color = mk_color(str_subset(names(concepts_list$regex_df), "concept"), "regex"),
      halign = "left"
    ),
    token_match = list(color = mk_color(.cv, .tv)),
    token_count = list(color = mk_color(.cv, .tv)),
    token_exclusion = list(
      color = mk_color(.cv, .tv),
      visible = data_sheets$token_exclusion$visible
    ),
    concept_count = list(color = mk_color(.cv)),
    text_replace = list(halign = "left"),
    text_regex = list(
      color = mk_color(.cv, .tv),
      halign = "left",
      max_width = 150
    ),
    text_match = list(color = mk_color(.cv, "match")),
    text_count = list(color = mk_color(.cv, "match")),
    mismatch = list(
      color = mk_color(.cv, "match"),
      visible = data_sheets$mismatch$visible
    ),
    params = list(halign = "left")
  )

  get_sheet_data <- \(name) {

    if (name == "token_exclusion") return(data_sheets$token_exclusion$rows)
    if (name == "mismatch") return(data_sheets$mismatch$rows)

    data_sheets[[name]]

  }

  reduce2(
    names(sheet_configs),
    sheet_configs,
    \(wb, name, cfg) {
      args <- compact(list(
        x = wb,
        sheet = name,
        data = get_sheet_data(name),
        color = cfg$color,
        halign = cfg$halign,
        max_width = cfg$max_width,
        visible = cfg$visible
      ))
      do.call(wb_add_custom, args)
    },
    .init = wb_workbook()
  )

}

.extract_sheets_gt <- \(
  data_sheets,
  concepts_list,
  id,
  text_input,
  concept_color,
  text_color
) {

  list(
    data = list(
      id =
        data_sheets$data |>
          select(-c(
            matches(concepts_list$root),
            "concept",
            "extract"
          )) |>
          gt_custom(font_size = 11) |>
          gt::sub_missing() |>
          gt_text_align(),
      text =
        data_sheets$data |>
          select(
            "n",
            .env$id,
            matches(concepts_list$root),
            "concept",
            "extract"
          ) |>
          mutate(` ... ` = "...", .after = all_of(id)) |>
          gt_custom(font_size = 11) |>
          gt_text_align() |>
          gt_text_color(
            column = matches(c(concepts_list$root, "concept")),
            color = concept_color
          ) |>
          gt_text_color(
            column = "extract",
            color = text_color
          )
    ),
    token_regex =
      data_sheets$token_regex |>
        gt_custom(head = NULL) |>
        gt_text_color(column = matches("concept"),
                      color = concept_color) |>
        gt_text_color(column = "regex",
                      color = text_color) |>
        gt_code_font(column = "regex"),
    token_match =
      data_sheets$token_match |>
        gt_custom() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = text_input,
                      color = text_color),
    token_count =
      data_sheets$token_count |>
        gt_custom() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = text_input,
                      color = text_color),
    token_exclusion =
      data_sheets$token_exclusion$data |>
        gt_custom() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = text_input,
                      color = text_color) |>
        gt::sub_missing(),
    concept_count =
      data_sheets$concept_count |>
        gt_custom() |>
        gt::sub_missing() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color),
    text_replace =
      data_sheets$text_replace |>
        gt_custom(head = NULL) |>
        gt_code_font(),
    text_regex =
      data_sheets$text_regex |>
        gt_custom(head = NULL) |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = text_input,
                      color = text_color) |>
        gt_code_font(column = text_input),
    text_match =
      data_sheets$text_match |>
        gt_custom() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = "match",
                      color = text_color),
    text_count =
      data_sheets$text_count |>
        gt_custom() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = "match",
                      color = text_color),
    mismatch =
      data_sheets$mismatch$data |>
        gt_custom() |>
        gt_text_align() |>
        gt_text_color(column = "concept",
                      color = concept_color) |>
        gt_text_color(column = "match",
                      color = text_color) |>
        gt::sub_missing(),
    params =
      gt_custom(data = data_sheets$params, head = NULL)
  )

}
