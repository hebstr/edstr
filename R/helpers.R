easy_ano <- \(x,
              to_hash = NULL,
              to_hide = NULL,
              hash_trunc = 25,
              hide_pattern = "---") {

  .ano_hash_fun <- \(x_hash, to_hash) {

    hash_trunc <- as.character(hash_trunc)

    x_hash |>
      mutate(
        "{to_hash}" :=
          .data[[to_hash]] |>
            rlang::hash() |>
            str_remove_all(glue(".{{{hash_trunc}}}$")),
          .by = all_of(to_hash))

  }

  .ano_hide_fun <- \(x_hide) {

    x_hide |> mutate(across(matches(to_hide), ~ hide_pattern))

  }

  if (!is.null(to_hash)) {

    .ano_data <-
    names(x) |>
      str_subset(to_hash |> paste(collapse = "|")) |>
      reduce(.ano_hash_fun, .init = x)

    if (!is.null(to_hide)) {

      .ano_data <- .ano_hide_fun(.ano_data)

    }

  } else if (!is.null(to_hide)) {

    .ano_data <- .ano_hide_fun(x)

  } else {

    .ano_data <- x

  }

  return(.ano_data)

}

format_text <- \(text) {

  .format_content <- regex("(?<=\">).+(?=</p>)")
  .format_tags <- regex("</?[a-z]+/?>")

  text |>
    str_extract_all(.format_content) |>
    map_chr(
      ~ . |>
        str_replace_all(.format_tags, " ") |>
        paste(collapse = " ")
    ) |>
    str_squish() |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT")

}

check_config <- \(suffix = NULL) {

  id <- "edstr_dirname"

  if (is.null(getOption(id))) {

    cli_abort(c(
      "{.field {id}} n'est pas configur\u00e9",
      "i" = "Cr\u00e9er avec {.fn edstr_config}"
    ))

  } else {

    .dirname <- getOption('edstr_dirname')
    .filename <- getOption('edstr_filename')

    lst(
      dir = .dirname,
      file = str_glue("{.filename}_{suffix}"),
      save = str_glue("{.dirname}/{file}.rds")
    )

  }

}


cli_save <- \(data, config_file, config_save) {

  cli_progress_step("Enregistrement du fichier {.strong {config_file}}")

  saveRDS(data, file = config_save)

  cli_progress_done()

  cli_alert_success("Fichier {.strong {config_file}} enregistr\u00e9 dans {.path {here(config_save)}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Dimensions}")
  cli_ul()
    cli_ul()
    cli_li("{nrow(data)} documents")
    cli_li("{ncol(data)} variables")
    cli_end()
  cli_text("\n\n")
  cli_rule()

  invisible(gc())

  return(invisible(data))

}


cli_load <- \(dir, file, save) {

  cli_progress_step("Chargement du fichier {.strong {file}}")

  .load <- readRDS(save)

  cli_progress_done()

  cli_alert_success("Ficher {.strong {file}} charg\u00e9 depuis {.path {here(save)}}")
  cli_text("\n\n")
  cli_rule()

  return(invisible(.load))

}


cli_check <- \(config_file, fun_save, fun_load) {

  cli::cli_text("\n\n")
  cli::cli_alert_warning("Le fichier {.strong {config_file}} existe d\u00e9ja.")

  choix <- menu(
    choices = c(
      "\u00c9craser le fichier existant",
      "Charger le fichier existant",
      "Annuler"
    ),
    title = NULL
  )

  if (choix == 1) {

    fun_save

  } else if (choix == 2) {

    fun_load

  } else {

    cli::cli_abort("Op\u00e9ration annul\u00e9e.")

  }

}


gt_custom <- \(data,
               head = 10,
               font_size = 12,
               ...) {

  if (is.null(head)) {

    data <-
    data |>
      gt(id = "tbl-id") |>
      opt_css("
        #tbl-id .gt_table {
          font-variant-ligatures: none;
        }
      ")

  } else {

    data <- gt_preview(data, top_n = head)

  }

  data |>
    tab_options(table.font.names = c("luciole", "system-ui"),
                table.font.size = px(font_size),
                column_labels.border.top.color = "white",
                ...) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels())

}


gt_text_color <- \(x, column, color) {

    tab_style(data = x,
              style = cell_text(weight = "bold", color = color),
              locations = cells_body(column))

}


gt_code_font <- \(x, column = everything()) {

  tab_style(data = x,
            style = cell_text(font = "fira code"),
            locations = cells_body(column))

}


gt_text_align <- \(x,
                   column = everything(),
                   align = "center") {

  tab_style(data = x,
            style = cell_text(align = align),
            locations =
              list(cells_column_labels(column),
                    cells_body(column)))

}


set_data_csv <- \(
  data,
  text_input,
  text_color,
  text_background,
  pattern
) {

  .css <-
  paste0(
    "color:", text_color, ";",
    "background-color:", text_background, ";",
    "font-weight:bold;",
    "font-family:system-ui;",
    "padding:0.2rem;",
    "border-radius:5px;"
  )

  pattern_compiled <- regex(as.character(pattern))

  text_vector <- str_replace_all(
    string = data[[text_input]],
    pattern = pattern_compiled,
    replacement = glue("<span style='{.css}'>\\1</span>")
  )

  data[[text_input]] <- text_vector

  return(data)

}


wb_add_custom <- \(x,
                    sheet,
                    ...,
                    data,
                    max_width = 100,
                    halign = "center",
                    font_size = 8,
                    font_color = "#222222",
                    concept_var = NULL,
                    concept_color = NULL,
                    text_var = NULL,
                    text_color = NULL,
                    border_color = "#999999",
                    border_type = "thin") {

  local_options(list(openxlsx2.maxWidth = max_width))

  .dims <-
  list(
    full = wb_dims(x = data),
    data = wb_dims(x = data, select = "data"),
    cols = wb_dims(x = data, select = "col_names")
  )

  .colors <-
  list(
    border = wb_color(border_color),
    header = wb_color("grey90")
  )

  .xlsx_output <-
  x |>
    wb_add_worksheet(
      sheet = sheet,
      zoom = 105,
      ...
    ) |>
    wb_add_data_table(
      x = data,
      na.strings = NULL
    ) |>
    wb_add_font(
      dims = .dims$cols,
      size = font_size + 1,
      bold = TRUE
    ) |>
    wb_add_font(
      dims = .dims$data,
      size = font_size
    ) |>
    wb_add_fill(
      dims = .dims$cols,
      color = .colors$header
    ) |>
    wb_set_col_widths(
      cols = 1:ncol(data),
      widths = "auto"
    ) |>
    wb_add_cell_style(
      dims = .dims$full,
      horizontal = halign,
      vertical = "center",
      wrap_text = TRUE
    ) |>
    wb_add_border(
      dims = .dims$data,
      top_color = .colors$border,
      top_border = border_type,
      bottom_color = .colors$border,
      bottom_border = border_type,
      left_color = .colors$border,
      left_border = border_type,
      right_color = .colors$border,
      right_border = border_type,
      inner_hcolor = .colors$border,
      inner_hgrid = border_type,
      inner_vcolor = .colors$border,
      inner_vgrid = border_type
    )

  .add_font <- \(wb, vars, color) {

    .dims <- wb_dims(x = data, cols = vars, select = "data")
    .color <- wb_color(color)

    wb_add_font(
      wb = wb,
      dims = .dims,
      color = .color,
      size = font_size,
      bold = TRUE
    )

  }

  if (!is.null(concept_color)) {

    .xlsx_output <-
    .add_font(wb = .xlsx_output,
              vars = concept_var,
              color = concept_color)

  }

  if (!is.null(text_color)) {

    .xlsx_output <-
    .add_font(wb = .xlsx_output,
              vars = text_var,
              color = text_color)

  }

  return(.xlsx_output)

}
