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
#' @param concepts_collapse
#' @param concepts_intersect
#' @param starts_with_only
#' @param exclus_manual
#' @param exclus_auto_except
#' @param regex_replace
#' @param mismatch_data
#' @param xlsx_save
#' @param concept_color
#' @param text_color
#' @param text_background
#' @param dir_suffix
#' @param filename_suffix
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
                   concepts_collapse = FALSE,
                   concepts_intersect = FALSE,
                   starts_with_only = FALSE,
                   exclus_manual = NULL,
                   exclus_auto_except = NULL,
                   regex_replace = NULL,
                   mismatch_data = FALSE,
                   xlsx_save = TRUE,
                   concept_color = "#0099EE",
                   text_color = "red",
                   text_background = "#FFFF44",
                   dir_suffix = sample,
                   filename_suffix = sample) {

  if (!is.null(seed)) set.seed(seed)

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  config_str <- with(config, str)

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

  if (concepts_collapse) {

    concepts <- concepts |> map(paste, collapse = "|")

  } else {

    concepts <-
    concepts |>
      pluck_depth() |>
      seq() |>
      reduce(~ list_flatten(.), .init = concepts)

  }

  concepts_root <- names(concepts) |> str_remove("_.+") |> unique()

### ----------------------------------------------------------------------------

  if (is.character(data)) data <- get(data)

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

  tic("Full steps")

  cli_h1("edstr_extract")
  cli_text("\n\n")

### SAVE PARAMS ----------------------------------------------------------------

  filename <- config$file
  dirname <- glue("{filename}_extract")
  save_dir <- glue("{config$dir}/{dirname}")
  save_files <- dirname

  if (!is.null(dir_suffix)) save_dir <- glue("{save_dir}_{dir_suffix}")

  if (!is.null(filename_suffix)) save_files <- glue("{dirname}_{filename_suffix}")

  if (!file.exists(glue(save_dir))) dir.create(path = glue(save_dir))

  save_extract <- glue("{save_dir}/{save_files}")

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
               str_replace_all("\\.|'", " ") |>
               str_split("\n")) |>
    unnest(text_input) |>
    mutate(!!text_input :=
             get(text_input) |>
               str_extract("(?<=>).+(?=</\\w+>)") |>
               str_split("</?\\w+/?>")) |>
    unnest(text_input) |>
    filter(str_detect(get(text_input), "[:graph:]")) |>
    mutate(!!text_input :=
             get(text_input) |>
               iconv(from = "UTF-8", to = "ASCII//TRANSLIT"))

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

  concepts_str_end <- if (starts_with_only) "" else "\\S*$"

  concepts_str <- paste(concepts, collapse = "|")

  data_regex_ngrams <- glue("^({concepts_str}){concepts_str_end}")

  # LONG
  data_ngrams_match <-
  ngrams |>
    imap(~ data_ngrams[[.y]] |>
           filter(str_detect(get(text_input), data_regex_ngrams)) |>
           count(pick(text_input),
                 name = "match",
                 sort = TRUE)) |>
    set_names(ngrams)

  # LONG
  data_match <-
  names(concepts) |>
    map(~ ngrams |>
          imap(~ data_ngrams[[.y]] |>
                 select(id, group, text_input) |>
                 mutate(ngrams = .x) |>
                 filter(get(text_input) %in% pull(data_ngrams_match[[.y]], text_input))) |>
          list_rbind() |>
          mutate(concept =
                   if_else(str_starts(get(text_input), concepts[[.]]),
                           true = .,
                           false = NA),
                 .before = text_input) |>
          drop_na()) |>
    list_rbind()

  rm(data_ngrams); invisible(gc())

  if (nrow(data_match) == 0) cli_abort("{.strong No match}")

  data_ngrams_count <-
  data_match |>
    count(ngrams, concept, pick(text_input),
          name = "match",
          sort = TRUE)

  cli_progress_done()
  cli_text("\n\n")

### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusion}")

  if (!is.null(exclus_auto_except)) {

    data_match <- filter(data_match, !str_detect(get(text_input), exclus_auto_except))

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
         reduce(full_join, by = names(data_match)),
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

  regex_concepts <-
  tibble(concept = names(concepts),
         regex = unlist(concepts))

  regex_replace <-
  c("e(?!$)" = "[eéèë]",
    "(?<=o)i" = "[iïî]",
    "\\s" = "(<br/>)?\\\\s?(-|')?\\\\s?(<br/>)?") |>
    append(regex_replace)

  regex_replace_tbl <-
  tibble(pattern = names(as.list(regex_replace)),
         replace = regex_replace)

  data_regex <-
  data_count |>
    mutate(!!text_input :=
             str_replace_all(get(text_input), regex_replace)) |>
    pull(text_input) |>
    paste(collapse = "|")

  data_regex <- glue("(?i)\\b({data_regex})\\b")

  # LONG
  data_regex_match <-
  edstr_view(data = data,
             str = data_regex,
             id = id,
             quiet = TRUE)

  cli_progress_done()
  cli_text("\n\n")

### EXTRACTION -----------------------------------------------------------------

  cli_progress_step("{.strong Extraction}")

  # LONG
  data_extract <-
  lst(base =
        lst(data =
              data |>
                mutate(extract_text =
                         get(text_input) |>
                           str_extract_all(data_regex) |>
                           map_chr(~ paste0(., collapse = " ; "))),
            concept =
              data_id |>
                distinct(pick(id, group), concept) |>
                pivot_wider(names_from = concept,
                            values_from = concept) |>
                mutate(across(any_of(names(concepts)), ~ ifelse(!is.na(.), 1, 0))),
            extract =
              data_id |>
                distinct(pick(id, group, text_input)) |>
                mutate(extract_unique = str_flatten(get(text_input), " ; "),
                       .by = id,
                       .keep = "unused") |>
                distinct()) |>
          reduce(inner_join, by = c(id, group)),
      final = base)

  .intersect <-
  concepts_root |>
    map(~ data_extract$base |>
          filter(if_any(matches(.), ~ . == 1))) |>
    reduce(inner_join, by = id) |>
    pull(id)

  if (concepts_intersect) {

    data_extract$final <- data_extract$base |> filter(get(id) %in% .intersect)

  }

  data_extract$final <- data_extract$final |> rownames_to_column("n")

  not_empty_data <- nrow(data_extract$final) > 0

  cli_progress_done()
  cli_text("\n\n")

### MISMATCH -------------------------------------------------------------------

  cli_progress_step("{.strong Mismatch}")

  if (mismatch_data) {

    data_id_mismatch <-
    data[c(id, group, text_input)] |>
      filter(!get(id) %in% data_extract$base[[id]])

  } else data_id_mismatch <- NULL

  data_regex_match_conv <-
  data_regex_match |>
    mutate(match =
             match |>
               iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
               tolower() |>
               str_replace_all(c("-(<br/>)?|-?<br/>" = " ",
                                 "\\s+" = " ")))

  data_regex_mismatch <-
  list(loss =
         data_match |>
           select(id, match = !!text_input) |>
           anti_join(y = data_regex_match_conv,
                     by = c(id, "match")),
       excess =
         data_regex_match_conv |>
           anti_join(y = data_match |> select(id, match = !!text_input),
                     by = c(id, "match")))

  data_mismatch <-
  list(id = data_id_mismatch,
       regex = data_regex_mismatch)

  not_empty_mismatch <- \(x) nrow(data_regex_mismatch[[x]]) > 0

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

### XLSX OUTPUT ----------------------------------------------------------------

  if (xlsx_save) {

    cli_progress_step("{.strong XLSX output}")

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
                         zoom = 115,
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

    data_xlsx <- data_extract$final |> select(-text_input, -extract_text)

    .xlsx_output <-
    wb_workbook() |>
      wb_add_custom(sheet = if (concepts_intersect) "data ∩" else "data",
                    data = if (not_empty_data) data_xlsx else add_row(data_xlsx),
                    concept_var = unique(data_count$concept),
                    concept_color = concept_color,
                    text_var = "extract_unique",
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_concepts",
                    data = regex_concepts,
                    concept_color = concept_color,
                    halign = "left") |>
      wb_add_custom(sheet = "match_concepts",
                    data = data_summary$concept,
                    concept_color = concept_color) |>
      wb_add_custom(sheet = "match_text",
                    data = data_id,
                    concept_color = concept_color,
                    text_color = text_color) |>
      wb_add_custom(sheet = "match_count",
                    data = data_count |> select(-ngrams),
                    concept_color = concept_color,
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_ngrams",
                    data = tibble(regex = data_regex_ngrams),
                    width = 80,
                    halign = "left") |>
      wb_add_custom(sheet = "regex_replace",
                    data = regex_replace_tbl,
                    halign = "left") |>
      wb_add_custom(sheet = "regex_final",
                    data = tibble(regex = data_regex),
                    width = 80,
                    halign = "left") |>
      wb_add_custom(sheet = "regex_match",
                    data = data_regex_match,
                    text_var = "match",
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_count",
                    data = data_regex_match |> count(match, sort = TRUE),
                    text_var = "match",
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_loss",
                    data =
                      if (not_empty_mismatch("loss")) data_regex_mismatch$loss
                      else add_row(data_regex_mismatch$loss),
                    text_var = "match",
                    text_color = text_color) |>
      wb_add_custom(sheet = "regex_excess",
                    data =
                      if (not_empty_mismatch("excess")) data_regex_mismatch$excess
                      else add_row(data_regex_mismatch$excess),
                    text_var = "match",
                    text_color = text_color) |>
      wb_save(file = glue("{save_extract}.xlsx"))

    cli_progress_done()
    cli_text("\n\n")

  }

### HTML OUTPUT ----------------------------------------------------------------

  cli_progress_step("{.strong HTML output}")

  highlight <- \(var) {

    .css <-
    css(color = text_color,
        background.color = text_background,
        font.weight = "bold",
        font.family = "system-ui",
        padding = "0.2rem",
        border.radius = "5px")

    var |>
      str_replace_all(glue("(?={data_regex})"),
                      glue("<span style='{.css}'>")) |>
      str_replace_all(glue("(?<={data_regex})"),
                      "</span>")

  }

  if (not_empty_data) {

    data_output <-
    data_extract$final |>
      mutate(!!text_input := get(text_input) |> highlight()) |>
      mutate(by_pat = glue("({row_number()}/{max(row_number())})"),
             by_pat = if_else(by_pat == "(1/1)", "", by_pat),
             .by = group, .before = group) |>
      select(-extract_unique)

  } else data_output <- NULL

  cli_progress_done()
  cli_text("\n\n")

### SAVE DATA ------------------------------------------------------------------

  cli_progress_step("{.strong Save data}")

  data_save <-
  list(regex =
         lst(concepts = regex_concepts,
             replace = regex_replace_tbl,
             final = data_regex,
             match = data_regex_match),
       match =
         list(init = data_match,
              final = data_match_final),
       count =
         list(init = data_ngrams_count,
              final = data_count),
       exclus =
         list(match = data_match_exclus,
              count = data_count_exclus),
       mismatch = data_mismatch,
       summary = data_summary,
       data = data_extract,
       output = data_output)

  assign(glue(save_files),
         data_save,
         envir = .GlobalEnv)

  save(list = glue(save_files),
       file = glue("{save_extract}.RData"))

  if (not_empty_data) {

    write_excel_csv(x = data_output, file = glue("{save_extract}.csv"))

  }

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
      geom_bar(mapping = aes(x = ngrams, fill = concept),
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
  cli_concepts <- concepts_root |> paste(collapse = " ET ")

  cli_p_final <- label_percent(0.1)(nrow(data) / nrow(data_init))

  cli_n_id_final <- nrow(data_id |> filter(get(id) %in% .intersect))

  cli_n_match_base <- n_distinct(data_match[[id]])
  cli_p_match_base <- label_percent(0.1)(cli_n_match_base / nrow(data))

  cli_n_match_final <- n_distinct(data_match[[id]] %in% .intersect)
  cli_p_match_final <- label_percent(0.1)(cli_n_match_final / nrow(data))

  cli_n_extract_base <- nrow(data_extract$base)
  cli_p_extract_base <- label_percent(0.1)(cli_n_extract_base / nrow(data))

  cli_n_group_base <- n_distinct(data_extract$base[[group]])
  cli_p_group_base <- label_percent(0.1)(cli_n_group_base / n_distinct(data[[group]]))

  cli_n_extract_final <- nrow(data_extract$final)
  cli_p_extract_final <- label_percent(0.1)(cli_n_extract_final / nrow(data))

  cli_n_group_final <- n_distinct(data_extract$final[[group]])
  cli_p_group_final <- label_percent(0.1)(cli_n_group_final / n_distinct(data[[group]]))

  cli_n_mismatch <- nrow(data_id_mismatch)
  cli_p_mismatch <- label_percent(0.1)(cli_n_mismatch / nrow(data))

  cli_n_group_mismatch <- n_distinct(data_id_mismatch[[group]])
  cli_p_group_mismatch <- label_percent(0.1)(cli_n_group_mismatch / n_distinct(data[[group]]))

  cli_n_files <- sum(str_detect(list.files(save_dir), "\\.\\w+"))

  cli_alert_info("{.strong Documents}")
  cli_ul()
  cli_ul()
    cli_li("Total: {nrow(data_init)} {id}")
    if (!is.null(sample)) cli_li("Sample: {nrow(data)} {id} ({cli_p_final})")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Correspondances}")
  cli_ul()
    cli_li("Totales: {nrow(data_match)} parmi {cli_n_match_base} {id} ({cli_p_match_base} {id})")
    cli_li("Distinctes: {nrow(data_count)}")
    cli_end()

  if (nrow(data_count_exclus) > 0) {

    cli_text("\n\n")
    cli_alert_info("{.strong Exclusions}")
    cli_ul()
      cli_li("Automatiques: {nrow(data_match_exclus$auto)}")
      cli_li("Manuelles: {nrow(data_match_exclus$manual)}")
      cli_end()

  }

  if (mismatch_data) {

    cli_text("\n\n")
    cli_alert_info("{.strong Mismatch:} {cli_n_mismatch} {id}")

  }

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {nrow(data_id)} correspondances {if (concepts_intersect) 'totales'}}")
  cli_ul()
    cli_li("{cli_n_extract_base} {id} ({cli_p_extract_base} {id})")
    if (check_group) cli_li("{cli_n_group_base} {group} ({cli_p_group_base} {group})")
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
  cli_alert_success("{.strong {n_concepts} concept{?s} parent{?s}}")
  cli_ul()
    cli_li("{.strong {nrow(data_count)} correspondance{?s} distincte{?s}}")
    cli_end()

  if (concepts_intersect) {

    cli_text("\n\n")
    cli_rule()
    cli_text("\n\n")

    cli_alert_success("{.strong {cli_n_id_final} correspondance{?s} à l'intersection ({cli_concepts})}")

    if (not_empty_data) {

      cli_ul()
        cli_li("{cli_n_extract_final} {id} ({cli_p_extract_final} {id})")
        if (check_group) cli_li("{cli_n_group_final} {group} ({cli_p_group_final} {group})")
        cli_end()

    }

  }

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_files} fichier{?s} enregistré{?s} vers {.path {here(save_dir)}}}")
  cli_ul()
    cli_li("RData: {col_red(glue('{save_files}.RData'))}")
    if (not_empty_data) cli_li("CSV output: {col_red(glue('{save_files}.csv'))}")
    if (xlsx_save) cli_li("XLSX output: {col_red(glue('{save_files}.xlsx'))}")
    cli_end()

  cli_text("\n\n")
  cli_rule()

  invisible(gc())

}
