#' Title
#'
#' @param data
#' @param text_input
#' @param split
#' @param replace
#' @param sample
#' @param seed
#' @param filter
#' @param id
#' @param group
#' @param ngrams
#' @param nchar_max
#' @param concepts
#' @param wrap
#' @param limits
#' @param exclus_manual
#' @param exclus_auto_except
#' @param regex_replace
#' @param html_save
#' @param html_popup
#' @param xlsx_save
#' @param xlsx_popup
#' @param mismatch_save
#' @param str_view
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
                   split = with(config_str, split),
                   replace = with(config_str, replace),
                   sample = NULL,
                   seed = NULL,
                   filter = NULL,
                   id = "",
                   group = "",
                   ngrams = 1,
                   nchar_max = 8000,
                   concepts,
                   wrap = TRUE,
                   limits = c("start-end", "start", "end", "asis"),
                   exclus_manual = NULL,
                   exclus_auto_except = NULL,
                   regex_replace = "",
                   html_save = FALSE,
                   html_popup = FALSE,
                   xlsx_save = TRUE,
                   xlsx_popup = TRUE,
                   mismatch_save = FALSE,
                   str_view = TRUE,
                   concept_color = "#0099EE",
                   text_color = "red",
                   text_background = "#FFFFFF",
                   dir_suffix = sample,
                   filename_suffix = sample) {

  if (!is.null(seed)) set.seed(seed)

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  config_str <- with(config, str)

  if (length(concepts) > 1) {

    if (!rlang::is_named(concepts)) {

      cli_abort("{.arg concepts} must all be named")

    } else {

      concepts <- as.list(concepts)

    }

  } else if (!is.list(concepts)) {

    if (!rlang::is_named(concepts)) {

      concepts <- list("<concept>" = concepts)

    } else {

      concepts <- as.list(concepts)

    }

  }

  concepts <-
  concepts |>
    pluck_depth() |>
    seq() |>
    reduce(~ list_flatten(.), .init = concepts)

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

  unlink(file.path(save_dir, list.files(save_dir)), recursive = TRUE)

### FILTERS --------------------------------------------------------------------

  if (!is.null(sample)) data <- data[sample(nrow(data), sample), ]

  filter <- enexpr(filter)

  if (!is.null(filter)) data <- filter(data, !!filter)

### SPLIT ----------------------------------------------------------------------

  cli_progress_step("{.strong Split}")

  data_split <-
  data[c(id, group, text_input)] |>
    unnest_tokens(output = !!text_input,
                  input = !!text_input,
                  token = str_split,
                  pattern = str_u(split),
                  to_lower = FALSE)

  cli_progress_done()
  cli_text("\n\n")

### REPLACE --------------------------------------------------------------------

  cli_progress_step("{.strong Replace}")

  if (!is.list(replace)) replace <- list(replace)

  data_split <-
  data_split |>
    mutate(!!text_input :=
             reduce(replace,
                    str_replace_all,
                    .init = get(text_input)))

  cli_progress_done()
  cli_text("\n\n")

### TOKENIZE -------------------------------------------------------------------

  cli_progress_step("{.strong Tokenize}")

  data_clean <-
  data_split |>
    mutate(!!text_input :=
             get(text_input) |>
               iconv(from = "UTF-8",
                     to = "ASCII//TRANSLIT") |>
               str_replace_all("'", " "))

  data_ngrams <-
  ngrams |>
    map(~ data_clean |>
          unnest_tokens(output = !!text_input,
                        input = !!text_input,
                        token = "ngrams",
                        n = .))

  cli_progress_done()
  cli_text("\n\n")

### MATCH ----------------------------------------------------------------------

  cli_progress_step("{.strong Match}")

  set_limit <- \(x, y) glue("{x}({str_u(concepts)}){y}")

  limits <- arg_match(limits)

  switch(limits,
         "start-end" = .concept_str <- set_limit("^", "$"),
         "start" = .concept_str <- set_limit("^", ""),
         "end" = .concept_str <- set_limit("", "$"),
         "asis" = .concept_str <- set_limit("", ""))

  data_ngrams_match <-
  ngrams |>
    imap(~ data_ngrams[[.y]] |>
           filter(str_detect(get(text_input), .concept_str)) |>
           count(pick(text_input),
                 name = "match",
                 sort = TRUE)) |>
    set_names(ngrams)

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

  if (nrow(data_match) == 0) cli_abort("{.strong No match}")

  data_ngrams_count <-
  data_match |>
    count(ngrams, concept, pick(text_input),
          name = "match",
          sort = TRUE)

  cli_progress_done()
  cli_text("\n\n")

### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusions}")

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

### EXTRACT --------------------------------------------------------------------

  cli_progress_step("{.strong Extract}")

  regex_replace <- list2(!!glue("[e{regex_replace}]") := "\\\\w") |> unlist()

  data_regex <-
  list(chr = regex_replace,
       space = c("\\s" = "\\\\s?[:punct:]?[:symbol:]?\\\\s?")) |>
    reduce(str_replace_all,
           .init =
             paste(data_count[[text_input]],
                   collapse = "|"))

  data_regex <- glue("(?i)\\b({data_regex})\\b")

  set_wrap <- \(x) if (wrap) unique(sort(c(x - 1, x, x + 1))) else x

  .which_rows <-
  data_clean[[text_input]] |>
    str_detect(data_regex) |>
    which() |>
    set_wrap()

  data_extract <-
  lst(base =
        list2(data = data |> select(-text_input),
              concept =
                data_id |>
                  distinct(pick(id, group), concept) |>
                  pivot_wider(names_from = concept,
                              values_from = concept) |>
                  mutate(across(any_of(names(concepts)), ~ ifelse(!is.na(.), 1, 0))),
              expr =
                data_id |>
                  distinct(pick(id, group, text_input)) |>
                  mutate(extract = str_flatten(get(text_input), " ; "),
                         .by = id,
                         .keep = "unused") |>
                  distinct(),
              !!text_input :=
                data_split[.which_rows, ] |>
                  mutate(!!text_input := str_flatten(get(text_input), " ; "),
                         .by = id) |>
                  distinct()) |>
          reduce(inner_join, by = c(id, group)) |>
          mutate(nchar = nchar(get(text_input))),
      output =
        base |>
          select(id, group, any_of(names(concepts)), extract, text_input))

  data_exclus_nchar <- data_extract$base |> filter(nchar > nchar_max)

  data_extract$base <- data_extract$base |> filter(nchar <= nchar_max)

  set_summary <- \(var) {

    list(total = data_match,
         exclus_auto = data_match_exclus$auto,
         exclus_manual = data_match_exclus$manual,
         final = data_id,
         distinct = data_count) |>
      set_names(~ glue("match_{.}")) |>
      imap(~ .x |> count(pick(all_of(var)), name = .y))

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

### XLSX OUTPUT ----------------------------------------------------------------

  if (xlsx_save) {

    cli_progress_step("{.strong XLSX output}")

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

    .wb <-
    wb_workbook() |>
      wb_add_custom(sheet = "data",
                    data =
                      data_extract$base |>
                        select(-text_input) |>
                        mutate(nchar = "") |>
                        rename(commentaires = nchar),
                    concept_var = unique(data_count$concept),
                    concept_color = concept_color,
                    text_var = "extract",
                    text_color = text_color) |>
      wb_add_custom(sheet = "match",
                    data = data_id,
                    concept_color = concept_color,
                    text_color = text_color) |>
      wb_add_custom(sheet = "concepts",
                    data = data_count,
                    concept_color = concept_color,
                    text_color = text_color) |>
      wb_add_custom(sheet = "summary",
                    data = data_summary$concept,
                    concept_color = concept_color) |>
      wb_save(file = glue("{save_extract}.xlsx"))


    cli_progress_done()
    cli_text("\n\n")

  } else {

    xlsx_popup <- FALSE

  }

  if (xlsx_popup) wb_open(.wb)

### HTML OUTPUT ----------------------------------------------------------------

  set_reactable <- \(x) {

    reactable(data = x,
              height = "100%",
              defaultColDef = colDef(vAlign = "center", align = "center"),
              showSortable = TRUE,
              striped = TRUE,
              searchable = TRUE,
              filterable = TRUE,
              selection = "multiple",
              columns =
                list2(.selection = colDef(sticky = "left"),
                      .rownames =
                        colDef(name = "n°",
                               width = 40,
                               sticky = "left"),
                      !!id := colDef(width = 110),
                      !!group := colDef(width = 110),
                      extract =
                        colDef(html = TRUE,
                               minWidth = 200,
                               style =
                                 list(color = text_color,
                                      fontWeight = "bold")),
                      !!text_input :=
                        colDef(html = TRUE,
                               minWidth = 600,
                               style = list(textAlign = "justify"))),
              rownames = TRUE,
              highlight = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100, 200, 500, 1000),
              defaultPageSize = 100,
              theme =
                reactableTheme(style =
                                 list(fontFamily = "Luciole, system-ui",
                                      fontSize = "11px"),
                               borderColor = "#dfe2e5",
                               searchInputStyle = list(width = "100%"),
                               rowSelectedStyle = list(backgroundColor = "skyblue")))

  }

  if (html_popup) html_save <- TRUE

  if (html_save) {

    cli_progress_step("{.strong HTML output}")


    .highlight <-
    paste0(c(glue("color:{text_color}"),
             glue("background-color:{text_background}"),
             glue("font-weight:bold")),
           collapse = ";")

    data_output <-
    data_extract$output |>
      mutate(extract = extract |> str_replace_all(" ; ", "<br>"),
             !!text_input :=
               get(text_input) |>
                 str_replace_all(" ; ", "<br><br>") |>
                 str_replace_all(glue("(?={data_regex})"),
                                 glue("<span style='{.highlight}'>")) |>
                 str_replace_all(glue("(?<={data_regex})"),
                                 "</span>")) |>
      set_reactable()

    output <- glue("{save_extract}.html")

    save_html(html = data_output,
              file = output)

    if (html_popup) browseURL(output)

    cli_progress_done()
    cli_text("\n\n")

  } else {

    data_output <- NULL

  }

### SAVE DATA ------------------------------------------------------------------

  cli_progress_step("{.strong Save data}")

  data_save <-
  list(split = data_split[.which_rows, ],
       regex =
         lst(init =
               tibble(concept = names(concepts),
                      regex = map_chr(concepts, ~ glue("^({.})$"))),
             replace = regex_replace,
             final = data_regex,
             match =
               edstr_view(data = data,
                          str = final,
                          id = id,
                          quiet = TRUE)),
       match =
         list(init = data_match,
              final = data_match_final),
       count =
         list(init = data_ngrams_count,
              final = data_count),
       exclus =
         list(match = data_match_exclus,
              count = data_count_exclus,
              nchar = data_exclus_nchar),
       mismatch =
         data[c(id, group, text_input)] |>
           filter(!get(id) %in% data_extract$base[[id]]),
       summary = data_summary,
       data = data_extract,
       output = data_output)

  assign(glue(save_files),
         data_save,
         envir = .GlobalEnv)

  save(list = glue(save_files),
       file = glue("{save_extract}.RData"))

  write_excel_csv(x = data_extract$output,
                  file = glue("{save_extract}.csv"))

  cli_progress_done()
  cli_text("\n\n")

### MISMATCH OUTPUT ------------------------------------------------------------

  if (mismatch_save) {

    if (nrow(data_save$mismatch) > 0) {

      data_output_mismatch <-
      data_save$mismatch |>
        mutate(text = str_remove_all(text, "</p>\n<p>|<br/>")) |>
        set_reactable()

      mismatch_name <- "{filename}_mismatch.html"

      save_html(html = data_output_mismatch,
                file = glue("{save_dir}/{glue(mismatch_name)}"))

    }

  }

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
  list(regex = data_save$regex,
       ngrams = data_ngrams_match,
       count =
         list(total = data_save$count$final,
              ngrams = data_summary$ngrams,
              concepts = data_summary$concept,
              exclus = data_save$exclus$count,
              final = data_save$count$final,
              mismatch = data_save$mismatch))

  print(data_print)

  cli_rule()
  cli_text("\n\n")

### CLI ------------------------------------------------------------------------

  toc()
  cli_text("\n\n")

  cli_p_final <- label_percent(0.1)(nrow(data) / nrow(data_init))

  cli_n_match <- n_distinct(data_match[[id]])
  cli_p_match <- label_percent(0.1)(cli_n_match / nrow(data))

  cli_n_extract <- nrow(data_extract$base)
  cli_p_extract <- label_percent(0.1)(cli_n_extract / nrow(data))

  cli_n_group <- n_distinct(data_extract$base[[group]])
  cli_p_group <- label_percent(0.1)(cli_n_group / n_distinct(data[[group]]))

  cli_n_mismatch <- nrow(data_save$mismatch)
  cli_p_mismatch <- label_percent(0.1)(cli_n_mismatch / nrow(data))

  cli_n_group_mismatch <- n_distinct(data_save$mismatch[[group]])
  cli_p_group_mismatch <- label_percent(0.1)(cli_n_group_mismatch / n_distinct(data[[group]]))

  cli_n_files <- sum(str_detect(list.files(save_dir), "\\.\\w+"))

  cli_alert_info("{.strong Documents}")
  cli_ul()
  cli_ul()
    cli_li("Total: {nrow(data_init)} {id}")
    if (!is.null(sample)) cli_li("Sample: {nrow(data)} {id} ({cli_p_final})")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Matching}")
  cli_ul()
    cli_li("Total: {nrow(data_match)} from {cli_n_match} {id} ({cli_p_match} {id})")
    cli_li("Distinct: {nrow(data_count)}")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Exclusions}")
  cli_ul()
    cli_li("Auto: {nrow(data_match_exclus$auto)}")
    cli_li("Manual: {nrow(data_match_exclus$manual)}")
    cli_li("Over {nchar_max} characters: {nrow(data_exclus_nchar)}")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Mismatch:} {cli_n_mismatch}")

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {nrow(data_id)} matchs}")
  cli_ul()
    cli_li("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (check_group) cli_li("{cli_n_group} {group} ({cli_p_group} {group})")
    cli_end()

  if (cli_n_mismatch > 0) {

    cli_text("\n\n")
    cli_alert_success("{.strong {cli_n_mismatch} mismatchs}")
    cli_ul()
      cli_li("{cli_n_mismatch} {id} ({cli_p_mismatch} {id})")
      if (check_group) cli_li("{cli_n_group_mismatch} {group} ({cli_p_group_mismatch} {group})")
      cli_end()

  }

  cli_text("\n\n")
  cli_alert_success("{.strong {n_distinct(data_count$concept)} concept{?s}}")
  cli_ul()
    cli_li("{.strong {nrow(data_count)} distinct match{?s}}")
    cli_end()

  cli_text("\n\n")
  cli_alert_success("{.strong {cli_n_files} files saved in {.path {here(save_dir)}}}")
  cli_ul()
    cli_li("Data: {col_red(glue('{save_files}.Rdata'))}")
    cli_li("CSV output: {col_red(glue('{save_files}.csv'))}")
    if (html_save) cli_li("HTML output: {col_red(glue('{save_files}.html'))}")
    if (xlsx_save) cli_li("XLSX output: {col_red(glue('{save_files}.xlsx'))}")
    if (exists("data_output_mismatch")) {
      cli_li("Mismatch output: {col_red(glue(mismatch_name))}")
    }
    cli_end()

  cli_text("\n\n")
  cli_rule()

}
