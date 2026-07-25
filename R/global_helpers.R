br <- \() cli_text("\n\n")

.timed <- \(msg, expr) {
  tic(msg)
  on.exit(toc(log = TRUE, quiet = TRUE))
  expr
}

# opt-in sub-stage timer for perf campaigns: transparent (returns `expr`) unless
# `edstr_profile` is set, so production runs keep the clean per-stage log
.timed_sub <- \(msg, expr) {
  if (!isTRUE(getOption("edstr_profile", FALSE))) {
    return(expr)
  }

  tic(msg)
  on.exit(toc(log = TRUE, quiet = TRUE))
  expr
}

label_pct <- \(x) paste0(round(x * 100, 1), "%")

read_query <- \(query, call = rlang::caller_env()) {
  if (!fs::file_exists(query)) {
    if (str_ends(query, "\\.sql")) {
      cli_abort("{.arg query}: file {.path {query}} not found", call = call)
    }
    return(query)
  }

  # the alternation consumes a quoted literal before its content can be read as
  # a comment marker, so `LIKE '%--%'` and `'/*'` survive the stripping
  .strip_comments <- \(x, comment) {
    str_replace_all(
      x,
      paste0("'[^']*'|", comment),
      \(m) ifelse(str_detect(m, "^'"), m, "")
    )
  }

  lines <- read_lines(file = query, skip_empty_rows = TRUE)
  lines <- .strip_comments(lines, "--.*$")

  block <- .strip_comments(str_flatten(lines, "\n"), "/\\*[\\s\\S]*?\\*/")

  lines <- str_split_1(block, "\n")
  lines <- lines[str_detect(lines, "\\S")]

  if (length(lines) == 0) {
    cli_abort(
      message = c(
        "{.arg query}: file {.strong {.path {query}}} is empty or contains only comments"
      ),
      call = call
    )
  }

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

easy_ano <- \(
  x,
  to_hash = NULL,
  to_hide = NULL,
  hash_len = 16,
  hide_pattern = "---"
) {
  # `NA` is the absence of a value, not a value: hashing it would give every
  # unknown identity one shared pseudonym, which a join then treats as a match
  .ano_hash_fun <- \(x_hash, to_hash) {
    x_hash |>
      mutate(
        "{to_hash}" := map_chr(
          .data[[to_hash]],
          ~ if (is.na(.x)) {
            NA_character_
          } else {
            str_sub(rlang::hash(.x), 1L, hash_len)
          }
        )
      )
  }

  # `matches()` would select through `grep()`, a different regex engine from the
  # one the callers validate the pattern against
  .ano_hide_fun <- \(x_hide) {
    hide_cols <-
      names(x_hide) |>
      str_subset(regex(paste(to_hide, collapse = "|"), ignore_case = TRUE))

    x_hide |> mutate(across(all_of(hide_cols), ~hide_pattern))
  }

  if (!is.null(to_hash)) {
    .ano_data <-
      names(x) |>
      str_subset(regex(paste(to_hash, collapse = "|"), ignore_case = TRUE)) |>
      reduce(.ano_hash_fun, .init = x)

    if (!is.null(to_hide)) {
      .ano_data <- .ano_hide_fun(.ano_data)
    }
  } else if (!is.null(to_hide)) {
    .ano_data <- .ano_hide_fun(x)
  } else {
    .ano_data <- x
  }

  .ano_data
}

easy_replace <- \(data, pattern, text) {
  if (!is.list(pattern)) {
    pattern <- list(pattern)
  }

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
  ...,
  error_empty = TRUE,
  match_only = FALSE,
  prep = NULL
) {
  data$match <- if (is.null(prep)) {
    str_extract_all(data[[text_input]], pattern)
  } else {
    .re2_extract_prepared(prep, pattern)
  }

  .data_match <- data |> unnest(match) |> select(all_of(id), "match")

  if (nrow(.data_match) == 0) {
    if (error_empty) {
      cli_abort(
        message = "{.strong No matches found}",
        call = rlang::caller_env()
      )
    }

    return(lst(
      match = .data_match,
      count = tibble(match = character(), n = integer()),
      text = character()
    ))
  }

  .data_count <- .data_match |> count(.data$match, sort = TRUE)

  if (match_only) {
    return(lst(match = .data_match, count = .data_count, text = NULL))
  }

  .matched_ids <- .data_match[[id]]

  .data_text <-
    data |>
    filter(.data[[id]] %in% .matched_ids) |>
    pull(text_input) |>
    str_view(pattern, ...)

  lst(
    match = .data_match,
    count = .data_count,
    text = .data_text
  )
}

gt_custom <- \(
  data,
  head = 10,
  font_size = 12,
  ...
) {
  if (is.null(head)) {
    data <-
      data |>
      gt::gt(id = "tbl-id") |>
      gt::opt_css(
        "
        #tbl-id .gt_table {
          font-variant-ligatures: none;
        }
      "
      )
  } else {
    data <- gt::gt_preview(data, top_n = head)
  }

  data |>
    gt::tab_options(
      table.font.names = c("luciole", "system-ui"),
      table.font.size = gt::px(font_size),
      column_labels.border.top.color = "white",
      ...
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )
}

gt_text_color <- \(x, column, color) {
  gt::tab_style(
    data = x,
    style = gt::cell_text(weight = "bold", color = color),
    locations = gt::cells_body(column)
  )
}

gt_code_font <- \(x, column = everything()) {
  gt::tab_style(
    data = x,
    style = gt::cell_text(font = "fira code"),
    locations = gt::cells_body(column)
  )
}

gt_text_align <- \(
  x,
  column = everything(),
  align = "center"
) {
  gt::tab_style(
    data = x,
    style = gt::cell_text(align = align),
    locations = list(
      gt::cells_column_labels(column),
      gt::cells_body(column)
    )
  )
}

# n private-use code points absent from `x`, used to hold a substring inert while
# the surrounding text is rewritten. Wingdings leaves BMP private-use characters
# in real source text, so the plane is chosen by inspection, not assumed free.
.pua_guard <- \(x, n, call = rlang::caller_env()) {
  planes <- c(0xF0000L, 0x100000L, 0xE000L)

  for (base in planes) {
    range <- paste0("[", intToUtf8(base), "-", intToUtf8(base + n - 1L), "]")

    if (!any(stri_detect_regex(x, range), na.rm = TRUE)) {
      return(intToUtf8(base + seq_len(n) - 1L, multiple = TRUE))
    }
  }

  cli_abort(
    message = c(
      "No free private-use range to rewrite {.arg x} safely",
      "i" = "Source text occupies every candidate plane"
    ),
    call = call
  )
}

set_class_css <- \(data, pattern, rows = NULL) {
  if (length(pattern) == 0) {
    return(data)
  }

  class_flatten <- \(str) {
    str_split_1(str, "_") |>
      accumulate(paste, sep = "-") |>
      paste(collapse = " ")
  }

  pattern_id <- map_chr(names(pattern), class_flatten)

  # case insensitivity is baked into the patterns by `regex_wrap`'s `(?i)`, not
  # set here: `regex()` defaults to a case-sensitive object anyway, and `map_chr`
  # would strip its class before `str_replace_all` could read any option
  pattern <- set_names(pattern, pattern_id) |> unlist()

  guard <- .pua_guard(data, length(pattern) + 1L)
  open <- guard[seq_along(pattern)]
  close <- guard[[length(guard)]]

  fun <- \(string, i) {
    idx <- if (is.null(rows)) TRUE else rows[[i]]

    if (!any(idx)) {
      return(string)
    }

    string[idx] <- str_replace_all(
      string = string[idx],
      pattern = pattern[[i]],
      replacement = paste0(open[[i]], "\\1", close)
    )

    string
  }

  markup <- c(
    str_glue("<span class='extract {names(pattern)}'>"),
    "</span>"
  )

  # a fixed-string search skips default-ignorable code points, so it deletes any
  # U+FEFF it passes over: the restore runs on escaped code points instead, and
  # the markup has `$` and `\` escaped out of it, the first introducing a
  # backreference on the replacement side, the second escaping it
  stri_replace_all_regex(
    str = reduce(seq_along(pattern), fun, .init = data),
    pattern = map_chr(c(open, close), .re2_char_class),
    replacement = stri_replace_all_regex(markup, "([\\\\$])", "\\\\$1"),
    vectorize_all = FALSE
  )
}

# shares its styling core with hebstr::.xlsx_add_sheet; keep the two in sync
# (edstr keeps `...`/`visible` and per-sheet config, hebstr keeps `get_xlsx`)
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

  # wb_dims(select = "data") collapses onto the header row on a zero-row frame,
  # so every data-scoped style has to be gated or it repaints the header
  has_data <- nrow(data) > 0L

  if (has_data && length(color)) {
    color <- color |>
      map(~ intersect(.x, names(data))) |>
      compact()
  }

  params <- list(
    dims = list(
      full = wb_dims(x = data),
      data = wb_dims(x = data, select = "data"),
      cols = wb_dims(x = data, select = "col_names"),
      proto = wb_dims(
        rows = seq_len(min(nrow(data) + 1L, 2L)),
        cols = seq_len(ncol(data))
      )
    ),
    colors = list(
      border = wb_color(border_color),
      header = wb_color(header_color)
    )
  )

  add_data_font <- \(wb) {
    if (!has_data) {
      return(wb)
    }

    wb_add_font(
      wb = wb,
      dims = params$dims$data,
      size = font_size
    )
  }

  add_color <- \(wb, vars, color) {
    if (!has_data) {
      return(wb)
    }

    wb_add_font(
      wb = wb,
      dims = wb_dims(x = data, cols = vars, select = "data"),
      color = wb_color(color),
      size = font_size,
      bold = TRUE
    )
  }

  # openxlsx2 scales quadratically on a wide border range, so the border is
  # resolved on two prototype rows then broadcast. The header row is homogeneous,
  # so it takes one style in bulk; data cells carry each column's number format,
  # hence one prototype per column there
  spread_header <- \(wb) {
    wb_set_cell_style(
      wb,
      dims = params$dims$cols,
      style = wb_get_cell_style(wb, dims = wb_dims(rows = 1L, cols = 1L))
    )
  }

  spread_data <- \(wb, col) {
    wb_set_cell_style(
      wb,
      dims = wb_dims(x = data, cols = col, select = "data"),
      style = wb_get_cell_style(
        wb,
        dims = wb_dims(rows = if (has_data) 2L else 1L, cols = col)
      )
    )
  }

  wb_add_worksheet(
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
    add_data_font() |>
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
      dims = params$dims$proto,
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
    spread_header() |>
    reduce(
      .x = seq_len(ncol(data)),
      .f = spread_data,
      .init = _
    ) |>
    reduce2(
      .x = color,
      .y = names(color),
      .f = add_color,
      .init = _
    )
}

.gc_r_java <- \() {
  gc(full = TRUE)

  rJava::J("java.lang.Runtime")$getRuntime()$gc()

  invisible(NULL)
}
