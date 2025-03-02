#' Title
#'
#' @param data
#' @param text_input
#' @param sample
#' @param seed
#' @param filter
#' @param id
#' @param group
#' @param ngrams
#' @param concepts
#' @param collapse
#' @param intersect
#' @param starts_with_only
#' @param exclus_manual
#' @param exclus_auto_escape
#' @param regex_replace
#' @param mismatch_data
#' @param to_xlsx
#' @param to_csv
#' @param concept_color
#' @param text_color
#' @param text_background
#' @param dirname_suffix
#' @param filename_suffix
#' @param load
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_extract <- \(data = glue("{with(config, file)}_clean"),
                   text_input = with(config, text),
                   sample = NULL,
                   seed = NULL,
                   filter = NULL,
                   id = "",
                   group = "",
                   ngrams = 1,
                   concepts,
                   collapse = FALSE,
                   intersect = FALSE,
                   starts_with_only = TRUE,
                   exclus_manual = NULL,
                   exclus_auto_escape = NULL,
                   regex_replace = NULL,
                   mismatch_data = FALSE,
                   to_xlsx = TRUE,
                   to_csv = TRUE,
                   concept_color = "#0099EE",
                   text_color = "red",
                   text_background = "#FFFF44",
                   dirname_suffix = if (!is.null(sample)) glue("sample_{sample}") else NULL,
                   filename_suffix = dirname_suffix,
                   load = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  config_dir <- config$dir
  config_str <- config$str
  filename <- config$file
  dirname <- "extract"

  save_dir <- glue("{config_dir}/{dirname}")

  if (!is.null(dirname_suffix)) save_dir <- glue("{save_dir}_{dirname_suffix}")

  save_files <- glue("{filename}_{dirname}")

  if (!is.null(filename_suffix)) save_files <- glue("{save_files}_{filename_suffix}")

  if (!file.exists(save_dir) && !load) dir.create(path = save_dir)

  save_extract <- glue("{save_dir}/{save_files}")

  save_extract_rdata <- glue("{save_extract}.RData")

  tic("Full steps")

  cli_h1("edstr_extract")
  cli_text("\n\n")

  if (!load) {

  if (!exists(data)) {

    cli_error_data(data = data,
                   fun = "clean")

  }

  if (is.character(data)) data <- get(data)

### CONCEPTS -------------------------------------------------------------------

  if (length(concepts) > 1) {

    if (!is_named(concepts)) {

      cli_abort("Chaque concept doit être nommé")

    } else {

      concepts <- as.list(concepts)

    }

  } else if (!is.list(concepts)) {

    if (!is_named(concepts)) {

      concepts <- list("<concept>" = concepts)

    } else {

      concepts <- as.list(concepts)

    }

  }

  list_flatten_complete <- \(data) {

    data |>
      pluck_depth() |>
      seq() |>
      reduce(~ list_flatten(.), .init = data)

  }

  if (collapse) {

    if (length(concepts) == 1) {

      cli_abort("Pas de regroupement possible si un seul concept est recherché")

    }

    if (pluck_depth(concepts) > 2) {

      concepts <-
      concepts |>
        map(~ . |>
              list_flatten_complete() |>
              paste(collapse = "|"))

    } else {

      concepts <-
      concepts |>
        paste(collapse = "|") |>
        set_names("<concept>")

    }

  } else {

    concepts <- list_flatten_complete(concepts)

  }

  concepts_keys <- names(concepts)

  concepts_names <- concepts |> imap(~ { if (is_named(.x)) names(.x) else .y })

  concepts_root <- concepts_keys |> str_remove("_.+") |> unique()

  if (length(concepts_root) == 1 && intersect) {

    cli_abort("Pas d'intersection possible si un seul concept est recherché")

  }

  str_concepts_comma <- str_flatten_comma(concepts_root)
  str_concepts_inter <- glue("[{paste(concepts_root, collapse = ' ET ')}]")

### ID -------------------------------------------------------------------------

  data_init <- data

  check_group <- group %in% names(data_init)

  if (!id %in% names(data_init)) {

    id <- "n_id"
    data <- data |> rownames_to_column(id)

  }

  if (!check_group) {

    group <- "n_group"
    data <-
    mutate(data,
           !!group := row_number(),
           .after = all_of(id))

  }

### FILTERS --------------------------------------------------------------------

  if (!is.null(sample)) data <- data[sample(nrow(data), sample), ]

  filter <- enexpr(filter)

  if (!is.null(filter)) data <- filter(data, !!filter)

### REPLACE --------------------------------------------------------------------

  cli_progress_step("{.strong Replace}")

  data_replace <-
  data[c(id, group, text_input)] |>
  mutate(!!text_input :=
           get(text_input) |>
             str_split("\n") |>
             map_chr(~ . |>
                       str_extract("(?<=>).+(?=</\\w+>)") |>
                       str_replace_all("\\.|'|</?\\w+/?>", " ") |>
                       na.omit() |>
                       paste(collapse = " ") |>
                       str_replace_all("\\s+", " ")) |>
             iconv(from = "UTF-8",
                   to = "ASCII//TRANSLIT"))

  cli_progress_done()
  cli_text("\n\n")

### TOKENIZE -------------------------------------------------------------------

  cli_progress_step("{.strong Tokenize}")

  data_ngrams <-
  ngrams |>
    map(~ data_replace |>
          unnest_tokens(output = !!text_input,
                        input = !!text_input,
                        token = "ngrams",
                        n = .) |>
          filter(str_detect(get(text_input), "[:alpha:]")))

  cli_progress_done()
  cli_text("\n\n")

  rm(data_replace); invisible(gc())

### MATCHING -------------------------------------------------------------------

  cli_progress_step("{.strong Matching}")

  concepts_regex_end <- if (starts_with_only) "\\S*$" else ""

  concepts_regex <- concepts |> map(~ glue("^({.}){concepts_regex_end}"))

  concepts_df <-
  tibble(concept_key = concepts_keys,
         concept_name = unlist(concepts_names),
         regex = unlist(concepts_regex))

  ngrams_extract <- \(x, n) {

    x |>
      imap(~ data_ngrams[[n]] |>
             mutate(!!text_input := str_extract(get(text_input), .x),
                    concept = .y, .before = text_input) |>
             drop_na())

  }

  data_ngrams_list <-
  ngrams |>
    map(~ concepts_regex |>
          ngrams_extract(.) |>
          list_rbind()) |>
    set_names(ngrams)

  data_ngrams_match <-
  data_ngrams_list |>
    map(~ . |>
          count(concept, pick(text_input),
                name = "match",
                sort = TRUE))

  data_match_init <-
  data_ngrams_list |>
    imap(~ mutate(.x, ngrams = .y)) |>
    list_rbind()

  .concepts_id <-
  concepts_root |>
    set_names() |>
    map(~ data_match_init |>
          distinct(pick(id, group), concept) |>
          pivot_wider(names_from = concept,
                      values_from = concept) |>
          filter(if_any(matches(.), ~ !is.na(.))) |>
          select(id, group))

  if (intersect) {

    .match_id <- .concepts_id |> reduce(inner_join, by = c(id, group))

  } else .match_id <- data_match_init[id]

  data_match <-
  data_match_init |>
    rename(concept_key = concept) |>
    mutate(concept = concept_key |> map_chr(~ concepts_names[[.]]),
           .after = concept_key) |>
    filter(get(id) %in% .match_id[[id]])

  data_match_init_df <-
  data |>
    select(-text_input) |>
    filter(get(id) %in% data_match_init[[id]])

  data_match_df <- data |> filter(get(id) %in% data_match[[id]])

  rm(data_ngrams); invisible(gc())

  if (nrow(data_match) == 0) {

    abort_intersect <- if (intersect) " à l'intersection" else ""

    cli_abort("{.strong Aucune correspondance{abort_intersect}}")

  }

  cli_progress_done()
  cli_text("\n\n")

### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusion}")

  if (!is.null(exclus_auto_escape)) {

    data_match <- filter(data_match, !str_detect(get(text_input), exclus_auto_escape))

  }

  set_exclus_auto <- \(regex, name) {

    data_match[[text_input]] |>
      unique() |>
      map(~ data_match |>
            filter(str_detect(get(text_input), glue(regex))) |>
            mutate(!!name := .)) |>
      list_rbind()

  }

  data_match_exclus <-
  list(auto =
         list(start = "^{.}\\s",
              end = "\\s{.}$",
              start_end = "{.}.+{.}$") |>
         imap(set_exclus_auto) |>
         reduce(full_join, by = names(data_match)) |>
         filter(ngrams > 2),
       manual =
         data_match |>
           filter(str_detect(get(text_input), exclus_manual %||% NA_character_))) |>
    imap(~ .x |>
           mutate(mode = .y, .before = everything()))

  .match_exclus <-
  list(concept =
         data_match_exclus |>
           list_rbind() |>
           pull(text_input) |>
           unique(),
       expr = expr(get(text_input) %in% .match_exclus$concept))

  data_match_final <-
  list(keep = data_match |> filter(!eval(.match_exclus$expr)),
       drop = data_match |> filter(eval(.match_exclus$expr)))

  data_count <-
  data_match_final |>
    map(~ list2(match = .,
                !!id := distinct(., pick(-group)),
                !!group := distinct(., pick(-id))) |>
          imap(~ . |>
                 count(ngrams, concept, pick(text_input),
                       name = .y,
                       sort = TRUE)) |>
          reduce(left_join, by = c("concept", text_input, "ngrams")))

  data_count_exclus <-
  data_match_exclus |>
    map(~ . |> select(-id, -group)) |>
    list_rbind() |>
    distinct() |>
    relocate(ngrams, .before = concept) |>
    left_join(data_count$drop,
              by = c("ngrams", "concept", text_input))

  data_id <- data_match_final$keep
  data_count <- data_count$keep

  cli_progress_done()
  cli_text("\n\n")

### REGEX ----------------------------------------------------------------------

  cli_progress_step("{.strong Regex}")

  regex_replace <-
  c("e(?!$)" = "[eéèë]",
    "(?<=o)i" = "[iïî]",
    "\\s" = "(<br/>)?\\\\s?(-|')?\\\\s?(<br/>)?") |>
    append(regex_replace)

  regex_wrap <- "(?i)\\b({x})\\b"

  regex_replace_df <-
  tibble(pattern = names(as.list(regex_replace)),
         replace = regex_replace)

  data_regex_replace <-
  data_count |>
    arrange(concept) |>
    mutate(!!text_input :=
             str_replace_all(get(text_input), regex_replace))

  data_regex_str <-
  glue(regex_wrap,
       x =
         data_regex_replace |>
           pull(text_input) |>
           paste(collapse = "|"))

  data_regex_df <-
  data_regex_replace |>
    nest(text = text, .by = concept) |>
    mutate(!!text_input :=
             map_chr(get(text_input), ~ str_flatten(unlist(.), "|")),
           !!text_input :=
             glue(regex_wrap, x = get(text_input)))

  data_regex_list <-
  data_regex_df$text |>
    as.list() |>
    set_names(data_regex_df$concept)

  data_regex_match <-
  data_regex_list |>
    imap(~ data_match_df |>
           edstr_view(text_input = text_input,
                      str = .x,
                      id = id,
                      quiet = TRUE) |>
           mutate(concept = .y,
                  .before = match)) |>
    list_rbind()

  data_regex_count <- data_regex_match |> count(concept, match, sort = TRUE)

  cli_progress_done()
  cli_text("\n\n")

### EXTRACTION -----------------------------------------------------------------

  cli_progress_step("{.strong Extraction}")

  data_extract <-
  list(data =
         data_match_df |>
           mutate(extract_all =
                    get(text_input) |>
                      str_extract_all(data_regex_str) |>
                      map_chr(paste, collapse = " ; ")),
       concept_name =
         data_regex_list |>
           imap(~ data_match_df |>
                  mutate(extract_all = str_extract(get(text_input), .x),
                         concept = .y) |>
                  drop_na(extract_all) |>
                  select(id, group, concept)) |>
           list_rbind() |>
           nest(concept = concept) |>
           mutate(concept = map_chr(concept, ~ str_flatten(unlist(.), " ; "))),
       concept_dummy =
         data_id |>
           distinct(pick(id, group), concept_key) |>
           pivot_wider(names_from = concept_key,
                       values_from = concept_key) |>
           mutate(across(matches(concepts_root), ~ ifelse(is.na(.), 0, 1))),
       extract_unique =
         data_id |>
           distinct(pick(id, group, text_input)) |>
           nest(extract_unique = text_input) |>
           mutate(extract_unique =
                    map_chr(extract_unique, ~ str_flatten(unlist(.), " ; ")))) |>
    reduce(inner_join, by = c(id, group)) |>
    relocate(concept, extract_all, .before = extract_unique) |>
    rownames_to_column("n")

  cli_progress_done()
  cli_text("\n\n")

### MISMATCH -------------------------------------------------------------------

  cli_progress_step("{.strong Mismatch}")

  data_id_mismatch_base <- data[c(id, group)]

  if (mismatch_data) {

    data_id_mismatch <-
    data_id_mismatch_base |>
      filter(!get(id) %in% data_match_init[[id]])

  } else {

    data_id_mismatch <-
    data_id_mismatch_base |>
      filter(get(id) %in% NA_character_)

  }

  data_regex_match_conv <-
  data_regex_match |>
    mutate(match =
             match |>
               iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
               tolower() |>
               str_replace_all(c("-(<br/>)?|-?<br/>" = " ",
                                 "\\s+" = " ")))

  data_mismatch <-
  list(id = data_id_mismatch,
       regex =
         data_match |>
           select(id, concept, match = !!text_input) |>
           anti_join(y = data_regex_match_conv,
                     by = c(id, "concept", "match")))

  cli_progress_done()
  cli_text("\n\n")

### SUMMARY --------------------------------------------------------------------

  cli_progress_step("{.strong Summary}")

  set_summary <- \(var) {

    list(total = data_match,
         exclus_auto = data_match_exclus$auto,
         exclus_manual = data_match_exclus$manual,
         final = data_id,
         distinct = data_count) |>
      set_names(~ glue("match_{.}")) |>
      imap(~ .x |>
             count(pick(all_of(var)),
                   sort = TRUE,
                   name = .y))

  }

  data_summary <-
  list(ngrams = set_summary("ngrams"),
       concept =
         list(match = set_summary("concept"),
              id =
                list(id, group) |>
                  imap(~ summarise(data_id,
                                   !!. := n_distinct(get(.)),
                                   .by = concept))) |>
           list_flatten()) |>
    imap(~ reduce(., left_join, by = .y))

  cli_progress_done()
  cli_text("\n\n")

  unlink(file.path(save_dir, list.files(save_dir)), recursive = TRUE)

### XLSX DATA ------------------------------------------------------------------

  if (to_xlsx) {

    cli_progress_step("{.strong Save to xlsx}")

    wb_add_custom <- \(x,
                       sheet,
                       ...,
                       data,
                       width = "auto",
                       halign = "center",
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

      .xlsx_output <-
      x |>
        wb_add_worksheet(sheet = sheet,
                         zoom = 105,
                         ...) |>
        wb_add_data_table(x = data,
                          na.strings = NULL) |>
        wb_add_font(dims = wb_dims(x = data, select = "col_names"),
                    size = font_size + 1,
                    bold = TRUE) |>
        wb_add_font(dims = wb_dims(x = data, select = "data"),
                    size = font_size) |>
        wb_add_fill(dims = wb_dims(x = data, select = "col_names"),
                    color = wb_color("grey90")) |>
        wb_set_col_widths(cols = 1:ncol(data), widths = width) |>
        wb_add_cell_style(dims = wb_dims(x = data),
                          horizontal = halign,
                          vertical = "center",
                          wrap_text = TRUE) |>
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

        .xlsx_output <-
        .add_font(.xlsx_output,
                  concept_var,
                  concept_color)

      }

      if (!is.null(text_color)) {

        .xlsx_output <-
        .add_font(.xlsx_output,
                  text_var,
                  text_color)

      }

      return(.xlsx_output)

    }

    .xlsx_mismatch <-
      lst(data = data_mismatch$regex,
          rows = if (nrow(data) > 0) data else add_row(data),
          visible = if (nrow(data) > 0) "true" else "false")

    xlsx_output <-
    wb_workbook() |>
      wb_add_custom(sheet = if (intersect) "data ∩" else "data",
                    data = data_extract |> select(-text_input, -extract_all),
                    concept_var = c(data_id$concept_key, "concept"),
                    concept_color = concept_color,
                    text_var = "extract_unique",
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_concepts",
                    data = concepts_df,
                    concept_var = names(concepts_df) |> str_subset("concept"),
                    concept_color = concept_color,
                    halign = "left") |>
      wb_add_custom(sheet = "match_concepts",
                    data = data_summary$concept,
                    concept_color = concept_color) |>
      wb_add_custom(sheet = "match_text",
                    data = data_id |> select(-concept_key),
                    concept_color = concept_color,
                    text_color = text_color) |>
      wb_add_custom(sheet = "match_count",
                    data = data_count |> select(-ngrams),
                    concept_color = concept_color,
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_replace",
                    data = regex_replace_df,
                    halign = "left") |>
      wb_add_custom(sheet = "regex_final",
                    data = data_regex_df,
                    concept_color = concept_color,
                    halign = "left") |>
      wb_add_custom(sheet = "regex_match",
                    data = data_regex_match,
                    concept_color = concept_color,
                    text_var = "match",
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_mismatch",
                    data = .xlsx_mismatch$rows,
                    concept_color = concept_color,
                    text_var = "match",
                    text_color = text_color,
                    visible = .xlsx_mismatch$visible) |>
      wb_add_custom(sheet = "regex_count",
                    data = data_regex_count,
                    concept_color = concept_color,
                    text_var = "match",
                    text_color = text_color)

    data_xlsx <-
    xlsx_output |>
      wb_get_sheet_names() |>
      map(~ xlsx_output |>
            wb_to_df(sheet = .) |>
            as_tibble())

    xlsx_output |> wb_save(file = glue("{save_extract}.xlsx"))

    cli_progress_done()
    cli_text("\n\n")

  } else data_xlsx <- NULL

### CSV DATA -------------------------------------------------------------------

  if (to_csv) {

    cli_progress_step("{.strong Save to csv}")

    .css <-
    css(color = text_color,
        background.color = text_background,
        font.weight = "bold",
        font.family = "system-ui",
        padding = "0.2rem",
        border.radius = "5px")

    data_csv <-
    data_extract |>
      mutate(!!text_input :=
               get(text_input) |>
                 str_replace_all(glue("(?={data_regex_str})"),
                                 glue("<span style='{.css}'>")) |>
                 str_replace_all(glue("(?<={data_regex_str})"),
                                 "</span>")) |>
      select(-matches(concepts_root), -extract_unique)

    data_csv |> write_excel_csv(file = glue("{save_extract}.csv"))

    cli_progress_done()
    cli_text("\n\n")

  } else data_csv <- NULL

### ASSIGN DATA ----------------------------------------------------------------

  cli_progress_step("{.strong Save to RData}")

  data_save <-
  list(regex =
         list(concepts = concepts_df,
              replace = regex_replace_df,
              final = data_regex_df,
              match = data_regex_match),
       match =
         list(init = data_match,
              final = data_match_final),
       count =
         list(init = data_ngrams_match,
              final = data_count),
       exclus =
         list(match = data_match_exclus,
              count = data_count_exclus),
       mismatch = data_mismatch,
       summary = data_summary,
       data =
         list(base = data |> select(-text_input),
              match = data_match_init_df,
              extract = data_extract,
              xlsx = data_xlsx,
              csv = data_csv))

  assign(save_files,
         data_save,
         envir = .GlobalEnv)

  save(list = save_files,
       file = save_extract_rdata)

  cli_progress_done()
  cli_text("\n\n")

### PRINT ----------------------------------------------------------------------

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  ngrams_max <- n_distinct(data_count$ngrams)

  if (ngrams_max > 1) {

    data_plot <-
    data_count |>
      ggplot() +
      geom_bar(mapping =
                 aes(x = as.numeric(ngrams),
                     fill = concept),
               stat = "count") +
      scale_x_continuous(n.breaks = ngrams_max)

    print(data_plot)

  }

  data_print <-
  list(ngrams = data_ngrams_match,
       count =
         list(total = data_save$count$final,
              ngrams = data_summary$ngrams,
              concepts = data_summary$concept,
              exclus = data_save$exclus$count,
              final = data_save$count$final),
       regex = data_save$regex,
       mismatch = data_save$mismatch)

  print(data_print)

  cli_rule()
  cli_text("\n\n")

### CLI ------------------------------------------------------------------------

  toc()
  cli_text("\n\n")

  n_concepts <- length(concepts_root)

  cli_intersect <- if (intersect) " à l'intersection {str_concepts_inter}" else ""

  cli_n_id <- nrow(data_id |> filter(get(id) %in% .match_id[[id]]))
  cli_p_id <- label_percent(0.1)(nrow(data) / nrow(data_init))

  cli_n_match <- n_distinct(data_match_init[[id]])
  cli_p_match <- label_percent(0.1)(cli_n_match / nrow(data))

  cli_n_extract <- nrow(data_extract)
  cli_p_extract <- label_percent(0.1)(cli_n_extract / nrow(data))

  cli_n_group <- n_distinct(data_extract[[group]])
  cli_p_group <- label_percent(0.1)(cli_n_group / n_distinct(data[[group]]))

  cli_n_mismatch <- nrow(data_id_mismatch)
  cli_p_mismatch <- label_percent(0.1)(cli_n_mismatch / nrow(data))

  cli_n_group_mismatch <- n_distinct(data_id_mismatch[[group]])
  cli_p_group_mismatch <- label_percent(0.1)(cli_n_group_mismatch / n_distinct(data[[group]]))

  cli_n_files <- sum(str_detect(list.files(save_dir), "\\.\\w+"))

  cli_alert_info("{.strong Documents}")
  cli_ul()
  cli_ul()
    cli_li("Total : {nrow(data_init)} {id}")
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
    cli_alert_info("{.strong Mismatch:} {cli_n_mismatch} {id}")

  }

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {n_concepts} concept{?s} parent{?s} :} {str_concepts_comma}")
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_id} correspondance{?s}{glue(cli_intersect)}}")
  cli_ul()
    cli_li("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (check_group) cli_li("{cli_n_group} {group} ({cli_p_group} {group})")
    cli_end()

  if (mismatch_data && cli_n_mismatch > 0) {

    cli_text("\n\n")
    cli_alert_success("{.strong {cli_n_mismatch} mismatch{?es}}")
    cli_ul()
      cli_li("{cli_n_mismatch} {id} ({cli_p_mismatch} {id})")
      if (check_group) cli_li("{cli_n_group_mismatch} {group} ({cli_p_group_mismatch} {group})")
      cli_end()

  }

  cli_text("\n\n")
  cli_alert_success("{.strong {nrow(data_count)} correspondance{?s} distincte{?s}}")

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_info("{.strong Dossier de destination : {.path {here(save_dir)}}}")
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_files} fichier{?s} enregistré{?s}}")
  cli_ul()
    cli_li("RData : {col_red(glue('{save_files}.RData'))}")
    if (to_xlsx) cli_li("xlsx : {col_red(glue('{save_files}.xlsx'))}")
    if (to_csv) cli_li("csv : {col_red(glue('{save_files}.csv'))}")
    cli_end()

  cli_text("\n\n")
  cli_rule()

  invisible(gc())

  } else {

    cli_load(dir = save_dir,
             file = save_files,
             save = save_extract_rdata)

  }

}
