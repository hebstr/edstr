str_u <- \(...) {

  str_c(c(...), collapse = "|")

}


easy_replace <- \(...,
                  replace = "</>") {

  col_replace <- col_br_red(replace)
  col_replace <- glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
  c(...) |>
    map(~ list2('{glue("<p>.*({.}).*</p>")}' := replace) |>
          unlist())

  replace_list <- list2("(\n*{replace})+\n*" := col_replace)

  unlist(append(str_list, replace_list))

}


cli_error_config <- \(dest_dir = NULL,
                      dest_filename = NULL) {

  if (!is.null(dest_dir) && !is.null(dest_filename)) {

    if (!file.exists(dest_dir)) dir.create(path = dest_dir, recursive = TRUE)

    list(dir = str_remove(dest_dir, "/+$"),
         file = dest_filename)

  } else {

    cli_abort(c("Configuration file doesn't exists!",
                "i" = "Please create it first with {.fn edstr_config}"))

  }

}


cli_save <- \(data,
              config_file,
              config_save,
              prop_import = NULL) {

  cli_progress_step("Saving {.strong {config_file}}")

  assign(config_file, data, envir = .GlobalEnv)

  save(list = config_file, file = config_save)

  cli_progress_done()

  cli_alert_success("{.strong {config_file}} saved to {.path {config_save}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Dimensions}")
  cli_ul()
    cli_li("{nrow(data)} documents {prop_import}")
    cli_li("{ncol(data)} variables")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}


cli_load <- \(config_dir,
              config_file,
              config_save) {

  if (!file.exists(config_save)) {

    cli_abort(c("{.strong {config_file}} not found in {.path {config_dir}}",
                "i" =
                  "Please create {.strong {config_file}} first with {.field load = FALSE}
                  or change directory in {.fn edstr_config}"))

  }

  cli_progress_step("Loading {.strong {config_file}}")

  load(config_save, envir = .GlobalEnv)

  cli_progress_done()

  cli_alert_success("{.strong {config_file}} loaded from {.path {config_save}}")
  cli_text("\n\n")
  cli_rule()

}


wb_add_custom <- \(x,
                   sheet,
                   data,
                   font_size = 8,
                   font_color = "#222222",
                   concept_var = "concept",
                   concept_color = NULL,
                   text_var = with(config, text),
                   text_color = NULL,
                   border_color = "#999999",
                   border_type = "thin") {

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  .wb <-
  x |>
    wb_add_worksheet(sheet = sheet) |>
    wb_add_data_table(x = data,
                      na.strings = fmt_txt("")) |>
    wb_add_font(dims = wb_dims(x = data, select = "col_names"),
                size = font_size + 1,
                bold = TRUE) |>
    wb_add_font(dims = wb_dims(x = data, select = "data"),
                size = font_size) |>
    wb_add_fill(dims = wb_dims(x = data, select = "col_names"),
                color = wb_color("grey90")) |>
    wb_set_col_widths(cols = 1:ncol(data), widths = "auto") |>
    wb_add_cell_style(dims = wb_dims(x = data),
                      horizontal = "center",
                      vertical = "center") |>
    wb_add_border(dims = wb_dims(x = data),
                  top_color = wb_color(border_color),
                  top_border = border_type,
                  bottom_color = wb_color(border_color),
                  bottom_border = border_type,
                  left_color = wb_color(border_color),
                  left_border = border_type,
                  right_color = wb_color(border_color),
                  right_border = border_type,
                  inner_hcolor = wb_color(border_color),
                  inner_hgrid = border_type,
                  inner_vcolor = wb_color(border_color),
                  inner_vgrid = border_type)

  .add_font <- \(wb, vars, color) {

    wb_add_font(wb = wb,
                dims =
                  wb_dims(x = data,
                          cols = vars,
                          select = "data"),
                color = wb_color(color),
                size = font_size,
                bold = TRUE)

  }

  if (!is.null(concept_color)) {

    .wb <-
    .add_font(.wb,
              concept_var,
              concept_color)

  }

  if (!is.null(text_color)) {

    .wb <-
    .add_font(.wb,
              text_var,
              text_color)

  }

  return(.wb)

}
