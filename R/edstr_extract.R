#' Title
#'
#' @param data
#' @param sample
#' @param filter
#' @param llm
#' @param split
#' @param replace
#' @param join_by
#' @param group_by
#' @param extra_cols
#' @param ngrams
#' @param nchar_max
#' @param str
#' @param upper_only
#' @param str_suppl
#' @param limits
#' @param exclus_man
#' @param exclus_auto_except
#' @param highlight_bg
#' @param highlight_color
#' @param dir_suffix
#' @param filename_suffix
#' @param config
#' @param config_str
#' @param config_concepts
#' @param text_input
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_extract <- \(data = glue::glue("{with(config, file)}_clean"),
                   sample = NULL,
                   filter = NULL,
                   llm = NULL,
                   split,
                   replace = NULL,
                   join_by,
                   group_by,
                   extra_cols = NULL,
                   ngrams,
                   nchar_max = 8000,
                   str,
                   upper_only = NA,
                   str_suppl = "\\t",
                   limits = "both",
                   exclus_man = "\\t",
                   exclus_auto_except = NA,
                   highlight_bg = "#ffffff",
                   highlight_color = "red",
                   dir_suffix = sample,
                   filename_suffix = sample,
                   config = get(.config_name),
                   config_str = with(config, str),
                   config_concepts = with(config, concepts),
                   text_input = with(config, text)) {

  cli_error_config()

  if (is.character(data)) data <- get(data)
  if (is.character(config)) config <- get(config)

  filter <- rlang::enexpr(filter)
  llm <- rlang::enexpr(llm)
  str <- with(config_concepts, eval(rlang::enexpr(str)))

  cli::cli_h1("edstr_extract")
  cli::cli_text("\n\n")

  #--- SAVE PARAMS ---------------------------------------------------------------------------------

  dir_name <- glue::glue("{with(config, file)}_extract")
  save_dir <- glue::glue("{with(config, dir)}/{dir_name}")
  save_files <- dir_name

  if (!is.null(dir_suffix)) save_dir <- glue::glue("{save_dir}_{dir_suffix}")

  if (!is.null(filename_suffix)) save_files <- glue::glue("{dir_name}_{filename_suffix}")

  if (!file.exists(glue::glue(save_dir))) dir.create(path = glue::glue(save_dir))

  .save_extract <- glue::glue("{save_dir}/{save_files}")

  .lib <- glue::glue("{save_dir}/lib")
  if (exists(.lib)) unlink(.lib, recursive = TRUE)

  #--- FILTERS -------------------------------------------------------------------------------------

  data_total <- data

  if (!is.null(sample)) {

    data <- data[sample(nrow(data), sample), ]

    cli::cli_alert_info("sample: {sample}")

    if (is.null(filter)) cli::cli_text("\n\n")

  } else sample <- nrow(data_total)

  if (!is.null(filter)) {

    data <- data |> dplyr::filter(!!filter)

    cli::cli_alert_info("filter on")
    cli::cli_text("\n\n")

  }

  if (!is.null(llm)) {

    llm_filter <-
    readr::read_csv(glue::glue(llm$filename)) |>
      dplyr::filter(!!llm$filter) |>
      dplyr::pull(!!llm$select)

    data <- data |> dplyr::filter(!!llm$select %in% llm_filter)

  }

  #--- SPLIT -------------------------------------------------------------------------------------

  cli::cli_progress_step("{.strong step 1:} split & replace")

  data_split <-
  data |>
    tidytext::unnest_tokens(output = !!text_input,
                            input = !!text_input,
                            token = stringr::str_split,
                            pattern = hebstr::str_u(with(config_str, eval(rlang::enexpr(split)))),
                            to_lower = FALSE) |>
    dplyr::mutate(!!text_input :=
                    purrr::reduce(with(config_str, !!rlang::enexpr(replace)),
                                  stringr::str_replace_all,
                                  .init = get(text_input)))

  cli::cli_progress_done()

  #--- LOWER, DEPUNCT AND TOKENIZE ----------------------------------------------------------------------------

  cli::cli_progress_step("{.strong step 2:} lower, depunct & tokenize")

  data_clean <-
  data_split |>
    dplyr::mutate(!!text_input :=
                    iconv(get(text_input),
                          from = "UTF-8",
                          to = "ASCII//TRANSLIT"))

  .ngrams <-
  ngrams |>
    purrr::map(~ data_clean |>
                 tidytext::unnest_tokens(output = !!text_input,
                                         input = !!text_input,
                                         token = "ngrams",
                                         n = .) |>
                 dplyr::distinct())

  if (!is.list(str)) str <- as.list(c("<concept>" = str))

  lim <- \(start, end) glue::glue("{start}({hebstr::str_u(str)}){end}")

  switch(limits,
         "start" = .cpts_str <- lim("^", ""),
         "end" = .cpts_str <- lim("", "$"),
         "both" = .cpts_str <- lim("^", "$"))

  .cpts_names <- stringr::str_c(names(str), collapse = ", ")

  .split <-
  seq_along(ngrams) |>
    purrr::map(~ .ngrams[[.]] |>
          dplyr::filter(stringr::str_detect(get(text_input), .cpts_str)) |>
          dplyr::count(!!.cpts_names := get(text_input), sort = TRUE))

  .match <-
  purrr::map2(.x = seq_along(ngrams),
       .y = ngrams,
       ~ .ngrams[[.x]][c(group_by, join_by, text_input)] |>
          dplyr::mutate(ngrams = .y) |>
          dplyr::filter(get(text_input) %in% c(.split[[.]][[!!.cpts_names]])))

  data_match <- dplyr::bind_rows(.match)

  if (nrow(data_match) == 0) cli::cli_abort("No results")

  cli::cli_progress_done()

  #--- LIST EXCLUSIONS ------------------------------------------------------------------------------------------

  cli::cli_progress_step("{.strong step 3:} set exclusion lists")

  str_exclus <-
  data_match[text_input] |>
    dplyr::filter(!stringr::str_detect(!!text_input, glue::glue("^({hebstr::str_u(exclus_auto_except)})$"))) |>
    dplyr::distinct() |>
    dplyr::pull()

  str_exclus_bb <- glue::glue("\\b{str_exclus}\\b")
  str_exclus_.sb <- glue::glue(".\\s{str_exclus}\\b")

  data_exclus <-
  data_match |>
    dplyr::select(dplyr::all_of(text_input), ngrams) |>
    dplyr::distinct()

  exclus_auto_start <-
  purrr::map2_df(.x = str_exclus_bb,
                 .y = str_exclus,
                 ~ data_exclus |>
                   dplyr::filter(stringr::str_starts(get(text_input), .x) &
                                 !stringr::str_ends(get(text_input), .x)) |>
                   dplyr::mutate(start_match = .y))

  exclus_auto_end <-
  purrr::map2_df(.x = str_exclus_bb,
                 .y = str_exclus,
                 ~ data_exclus |>
                   dplyr::filter(!stringr::str_starts(get(text_input), .x) &
                                 stringr::str_ends(get(text_input), .x)) |>
                   dplyr::mutate(end_match = .y))

  exclus_auto_both <-
  purrr::pmap_df(list(str_exclus_bb,
                      str_exclus_.sb,
                      str_exclus),
                 ~ data_exclus |>
                   dplyr::filter(stringr::str_starts(get(text_input), ..1) &
                                 stringr::str_ends(get(text_input), ..2)) |>
                   dplyr::mutate(both_match = ..3))

  data_exclus_auto <-
  exclus_auto_start |>
    dplyr::full_join(exclus_auto_end,
                     by = c(text_input, "ngrams"),
                     relationship = "many-to-many") |>
    dplyr::full_join(exclus_auto_both,
                     by = c(text_input, "ngrams"),
                     relationship = "many-to-many")

  if (nrow(data_exclus_auto) > 0) {

    data_exclus_auto <-
    data_exclus_auto |>
      dplyr::filter(!stringr::str_detect(end_match, hebstr::str_u(data_exclus_auto[[text_input]])) |
                    is.na(end_match)) |>
      dplyr::filter(!stringr::str_detect(start_match, hebstr::str_u(data_exclus_auto[[text_input]])) |
                    is.na(start_match))

  }

  data_match_final <-
  data_match |>
    dplyr::anti_join(data_exclus_auto[text_input],
                     by = text_input)

  data_exclus_man <-
  data_match_final |>
    dplyr::filter(stringr::str_detect(get(text_input), hebstr::str_u(exclus_man)))

  data_match_final <-
  data_match_final |>
    dplyr::anti_join(data_exclus_man[text_input],
                     by = text_input)

  cli::cli_progress_done()

### EXTRACT --------------------------------------------------------------------------------

  cli::cli_progress_step("{.strong step 4:} extract")

  data_id <-
  names(str) |>
    purrr::map(~ data_match_final |>
                 dplyr::mutate(concept =
                                 ifelse(stringr::str_starts(get(text_input),
                                                            hebstr::str_u(str[[.]])), ., NA))) |>
    dplyr::bind_rows() |>
    tidyr::drop_na()

  data_count <-
  data_id |>
    dplyr::count(!!text_input := get(text_input), concept, ngrams,
                 sort = TRUE)

  data_extract <-
  data_count[[text_input]] |>
    stringr::str_replace_all(glue::glue("\\b{upper_only}\\b"),
                             glue::glue("(?-i){toupper(upper_only)}"))

  data_extract <- stringr::str_c(data_extract, collapse = "\\b|(?i)\\b")
  data_extract <- glue::glue("(?i)\\b{data_extract}\\b")

  data_extract_replace <-
  c("e|(?<=m(a|i)cro)\\s" = ".",
    "\\sa\\s" = " . ")

  data_extract <-
  purrr::reduce(list(data_extract_replace),
                stringr::str_replace_all,
                .init = data_extract)

  .cpts_label <- stringr::str_unique(data_id$concept)

  .rows <-
  list(str = data_extract,
       str_suppl = str_suppl) |>
    purrr::map(~ data_clean[[text_input]] |>
                 stringr::str_detect(.) |>
                 which())

  data_str_br_db <-
  data_split[sort(c(.rows$str_suppl, .rows$str)), ] |>
    dplyr::inner_join(data_id[names(data_id) != text_input],
                      by = c(join_by, group_by),
                      relationship = "many-to-many") |>
    dplyr::select(dplyr::all_of(c(join_by,
                                  group_by,
                                  extra_cols,
                                  text_input)),
                  concept) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = concept,
                       values_from = concept) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(.cpts_label),
                                ~ ifelse(!is.na(.), 1, NA))) |>
    dplyr::group_by(!!join_by := get(join_by)) |>
    dplyr::mutate(n_row = seq_along(text_input)) |>
    tidyr::fill(dplyr::starts_with(.cpts_label),
                .direction = "updown") |>
    dplyr::mutate(!!text_input :=
                    get(text_input) |>
                      stringr::str_flatten("<br><br>"),
                  dplyr::across(dplyr::starts_with(.cpts_label),
                                ~ tidyr::replace_na(., 0)),
                  nchar = nchar(get(text_input))) |>
    dplyr::ungroup() |>
    dplyr::select(-n_row) |>
    dplyr::filter(!is.na(get(group_by))) |>
    dplyr::distinct()

  data_exclus_nchar <-
  data_str_br_db[c(text_input, "nchar")] |>
    dplyr::filter(nchar > nchar_max)

  data_str_br <-
  data_str_br_db |>
    dplyr::group_by(!!group_by := get(group_by),
                    !!text_input := get(text_input)) |>
    dplyr::filter(!duplicated(get(text_input)),
           nchar <= nchar_max) |>
    dplyr::ungroup()

  data_str_cat <-
  data_str_br |>
    dplyr::mutate(!!text_input :=
                    stringr::str_replace_all(get(text_input), "<br><br>", " "))

  data_str_split <-
  data_str_cat |>
    tidyr::separate_rows(dplyr::all_of(text_input), sep = "(?<=\\.)\\s")

  cli::cli_progress_done()

  #--- SUMMARIZE AND SET FINAL LIST -----------------------------------------------------------

  cli::cli_progress_step("{.strong step 5:} summarize & set final list")

  summary_cols <-
  list(match = data_match,
       exclus_auto = data_exclus_auto,
       exclus_man = data_exclus_man,
       retenu = data_id,
       distinct = data_count)

  data_summary <-
  purrr::map2(.x = summary_cols,
              .y = names(summary_cols),
              ~ .x |>
                dplyr::group_by(ngrams) |>
                dplyr::count(name = .y))

  data_summary <-
  seq(summary_cols) |>
    purrr::map_df(~ tibble::tibble(ngrams = ngrams) |>
                    dplyr::left_join(data_summary[[.]], by = "ngrams")) |>
    dplyr::group_by(ngrams) |>
    tidyr::fill(names(summary_cols), .direction = "updown") |>
    dplyr::distinct()

  data_match_list <-
  list(data = data,
       split = data_split,
       match = data_match,
       id = data_id,
       count = data_count,
       extract = data_extract,
       str = list(br = data_str_br,
                  cat = data_str_cat,
                  split = data_str_split),
       exclusions = list(auto = data_exclus_auto,
                         man = data_exclus_man,
                         nchar = data_exclus_nchar),
       summary = data_summary)

  assign(glue::glue(save_files),
         data_match_list,
         envir = .GlobalEnv)

  cli::cli_progress_done()

  ### PRINT ------------------------------------------------------------------------------------

  print(list(ngrams = .split))

  .extract_print(str,
                 data_id,
                 data_count,
                 concept,
                 group_by)

  print(list(exclus_auto = data_exclus_auto,
             exclus_man = data_exclus_man,
             summary = data_summary))

  #--- OUTPUT ------------------------------------------------------------------------------------

  cli::cli_progress_step("{.strong step 6:} output")

  h_color <- glue::glue("color:{highlight_color}")
  h_bg <- glue::glue("background-color:{highlight_bg}")

  rt_common <-
  list(args = list(height = "100%",
                   defaultColDef = reactable::colDef(vAlign = "center",
                                                     align = "center"),
                   showSortable = TRUE,
                   striped = TRUE,
                   searchable = TRUE,
                   filterable = TRUE),
       theme = list(style = list(fontFamily = "Segoe UI",
                                 fontSize = "11px"),
                    borderColor = "#dfe2e5",
                    stripedColor = "#e1f6ff",
                    searchInputStyle = list(width = "100%")))

  data_text <-
  data_str_br |>
    dplyr::mutate(!!text_input :=
                    get(text_input) |>
                      stringr::str_replace_all(glue::glue("(?={data_extract})"),
                                               glue::glue("<span style='{h_color};{h_bg}'> ")) |>
                      stringr::str_replace_all(glue::glue("(?<={data_extract})"),
                                               "</span></span>")) |>
    dplyr::select(-nchar) |>
    reactable::reactable(!!!rt_common$args,
                         selection = "multiple",
                         columns = rlang::list2(.selection = reactable::colDef(sticky = "left"),
                                                .rownames = reactable::colDef(name = "no",
                                                                              width = 40,
                                                                              sticky = "left"),
                                                !!text_input := reactable::colDef(html = TRUE,
                                                                                  minWidth = 500,
                                                                                  maxWidth = 500,
                                                                                  style = list(textAlign = "justify"))),
                         rownames = TRUE,
                         highlight = TRUE,
                         showPageSizeOptions = TRUE,
                         pageSizeOptions = c(10, 25, 50, 100, 200, 500, 1000),
                         defaultPageSize = 100,
                         theme = reactable::reactableTheme(!!!rt_common$theme,
                                                           rowSelectedStyle = list(backgroundColor = "skyblue"))) |> rlang::inject()

  data_cpts <-
  data_count |>
    dplyr::select(-ngrams) |>
    reactable::reactable(!!!rt_common$args,
                         columns = rlang::list2(!!text_input := reactable::colDef(name = "expression",
                                                                           style = list(color = highlight_color)),
                                                concept = reactable::colDef(maxWidth = 200),
                                                n = reactable::colDef(maxWidth = 75)),
                         defaultPageSize = 1000,
                         theme = reactable::reactableTheme(!!!rt_common$theme)) |> rlang::inject()

  data_to_output <- \(name, data) {

    output <- glue::glue("{.save_extract}_{name}.html")
    htmltools::save_html(data, output)
    utils::browseURL(output)

  }

  data_to_output("cpts", data_cpts)
  data_to_output("text", data_text)

  data_match_list <-
  append(data_match_list,
         list(output = list(cpts = data_cpts,
                            text = data_text)))

  cli::cli_progress_done()

  #--- SAVE -------------------------------------------------------------------------------------------------

  cli::cli_progress_step("{.strong step 7:} save")

  assign(glue::glue(save_files),
         data_match_list,
         envir = .GlobalEnv)

  save(list = glue::glue(save_files),
       file = glue::glue("{.save_extract}.RData"))

  readr::write_excel_csv(data_str_cat,
                         file = glue::glue("{.save_extract}_text.csv"))

  #data_match_list[c("id", "count", "exclusions", "summary")] |>
  #  list_flatten() |>
  #  write_xlsx(path = glue::glue(.save_extract, ".xlsx"))

  cli::cli_progress_done()

  cli_n_total <- nrow(data_total)
  cli_n_final <- nrow(data)
  cli_p_final <- scales::percent(cli_n_final / cli_n_total, accuracy = 0.1)
  cli_n_join <- dplyr::n_distinct(data_match[[join_by]])
  cli_p_join <- scales::percent(cli_n_join / cli_n_final, accuracy = 0.1)
  cli_n_distinct_match <- dplyr::n_distinct(data_match[[text_input]])
  cli_n_dupl <- nrow(data_str_br_db) - nrow(data_str_br)
  cli_n_extract <- dplyr::n_distinct(data_str_br[[join_by]])
  cli_p_extract <- scales::percent(cli_n_extract / cli_n_final, accuracy = 0.1)
  cli_n_concept <- dplyr::n_distinct(data_count$concept)
  cli_n_group <- dplyr::n_distinct(data_str_br[[group_by]])

  cli::cli_text("\n\n")
  cli::cli_rule()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Observations}")
  cli::cli_ul()
    cli::cli_li("Total: {cli_n_total}")
    cli::cli_li("Final: {cli_n_final} ({cli_p_final})")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Matching}")
  cli::cli_ul()
    cli::cli_li("Total: {nrow(data_match)} from {cli_n_join} {join_by} ({cli_p_join} final obs.)")
    cli::cli_li("Distinct: {cli_n_distinct_match}")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Exclusions}")
  cli::cli_ul()
    cli::cli_li("Auto: {nrow(data_exclus_auto)}")
    cli::cli_li("Manual: {nrow(data_exclus_man)}")
    cli::cli_li("Duplicates: {cli_n_dupl}")
    cli::cli_li("Over {nchar_max} chr.: {nrow(data_exclus_nchar)}")
    cli::cli_end()

  cli::cli_text("\n\n")
  cli::cli_rule()
  cli::cli_text("\n\n")
  cli::cli_alert_success("{nrow(data_id)} matchs from:")
  cli::cli_ul()
    cli::cli_li("{cli_n_extract} {join_by} ({cli_p_extract} final obs.)")
    cli::cli_li("{cli_n_group} {group_by}")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_success("{nrow(data_count)} distinct expressions from {cli_n_concept} concepts")

  cli::cli_text("\n\n")
  cli::cli_alert_success("files saved to {.path {save_dir}}")
  cli::cli_text("\n\n")
  cli::cli_rule()

}
