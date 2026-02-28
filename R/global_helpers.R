br <- \() cli_text("\n\n")

read_query <- \(query, call = rlang::caller_env()) {

  if (!fs::file_exists(query)) return(query)

  lines <- read_lines(file = query, skip_empty_rows = TRUE)
  lines <- str_remove(lines, "--.*$")

  block <- str_flatten(lines, "\n")
  block <- str_remove_all(block, "/\\*[\\s\\S]*?\\*/")

  lines <- str_split_1(block, "\n")
  lines <- lines[str_detect(lines, "\\S")]

  if (length(lines) == 0) cli_abort(
    message = c(
      "{.arg query} : le fichier {.strong {.path {query}}}
      est vide ou ne contient que des commentaires"
    ),
    call = call
  )

  cli_code(
    lines = str_flatten(lines, "\n"),
    language = "SQL"
  )

  str_flatten(lines, " ")

}

easy_flatten <- \(x) {

  seq_depth <- seq(pluck_depth(x))

  reduce(seq_depth, ~ list_flatten(.), .init = x)

}

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

easy_replace <- \(data, pattern, text) {

  if (!is.list(pattern)) pattern <- list(pattern)

  mutate(
    .data = data,
    !!text := reduce(
      .x = pattern,
      .f = str_replace_all,
      .init = .data[[text]]
    )
  )

}

view_output <- \(
  data,
  text_input,
  pattern,
  id,
  ...
) {

  data$match <- str_extract_all(data[[text_input]], pattern)

  .data_match <- data |> unnest(match) |> select(id, match)

  if (nrow(.data_match) == 0) {

    cli_abort(
      message = "{.strong Aucune correspondance}",
      call = rlang::caller_env()
    )

  }

  .data_count <- .data_match |> count(match, sort = TRUE)

  .data_text <-
  data |>
    filter(.data[[id]] %in% .data_match[[id]]) |>
    pull(text_input) |>
    str_view(pattern, ...)

  lst(
    match = .data_match,
    count = .data_count,
    text = .data_text
  )

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

set_class_css <- \(data, pattern) {

  pattern <-
  set_names(pattern, str_to_kebab) |>
    map_chr(~ regex(as.character(.)))

  fun <- \(string, regex, class) str_replace_all(
    string = string,
    pattern = regex,
    replace = str_glue("<span class='extract {class}'>\\1</span>")
  )

  reduce2(
    .x = pattern,
    .y = names(pattern),
    .f = fun,
    .init = data
  )

}

wb_add_custom <- \(
  x,
  sheet,
  data,
  max_width = 60,
  halign = "center",
  font_size = 8,
  header_color = "#E5E5E5",
  border_color = "#999999",
  border_type = "thin",
  color = NULL,
  ...
) {

  local_options(list(openxlsx2.maxWidth = max_width))

  params <- list(
    dims = list(
      full = wb_dims(x = data),
      data = wb_dims(x = data, select = "data"),
      cols = wb_dims(x = data, select = "col_names")
    ),
    colors = list(
      border = wb_color(border_color),
      header = wb_color(header_color)
    )
  )

  add_color <- \(wb, vars, color) wb_add_font(
    wb = wb,
    dims = wb_dims(x = data, cols = vars, select = "data"),
    color = wb_color(color),
    size = font_size,
    bold = TRUE
  )

  output <- wb_add_worksheet(
    wb = x,
    sheet = sheet,
    zoom = 105,
    ...
  ) |>
    wb_add_data_table(
      x = data,
      na.strings = NULL
    ) |>
    wb_add_font(
      dims = params$dims$cols,
      size = font_size + 1,
      bold = TRUE
    ) |>
    wb_add_font(
      dims = params$dims$data,
      size = font_size
    ) |>
    wb_add_fill(
      dims = params$dims$cols,
      color = params$colors$header
    ) |>
    wb_set_col_widths(
      cols = seq_len(ncol(data)),
      widths = "auto"
    ) |>
    wb_add_cell_style(
      dims = params$dims$full,
      horizontal = halign,
      vertical = "center",
      wrap_text = TRUE
    ) |>
    wb_add_border(
      dims = params$dims$full,
      top_color = params$colors$border,
      top_border = border_type,
      bottom_color = params$colors$border,
      bottom_border = border_type,
      left_color = params$colors$border,
      left_border = border_type,
      right_color = params$colors$border,
      right_border = border_type,
      inner_hcolor = params$colors$border,
      inner_hgrid = border_type,
      inner_vcolor = params$colors$border,
      inner_vgrid = border_type
    ) |>
    reduce2(
      .x = color,
      .y = names(color),
      .f = add_color,
      .init = _
    )

  return(output)

}


.gc_r_java <- \() {

  gc(full = TRUE)

  rJava::J("java.lang.Runtime")$getRuntime()$gc()

  invisible(NULL)

}
