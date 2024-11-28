#' Title
#'
#' @param data
#' @param sample
#' @param filter
#' @param text_input
#' @param split
#' @param replace
#' @param id
#' @param group
#' @param extra_cols
#' @param ngrams
#' @param nchar_max
#' @param concepts
#' @param concepts_suppl
#' @param wrap
#' @param wrap_suppl
#' @param upper_only
#' @param limits
#' @param union
#' @param exclus_man
#' @param exclus_auto_except
#' @param highlight_weight
#' @param highlight_color
#' @param highlight_bg
#' @param print
#' @param html_save
#' @param html_popup
#' @param dir_suffix
#' @param filename_suffix
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_extract <- \(data = glue("{with(config, file)}_clean"),
                   sample = NULL,
                   filter = NULL,
                   text_input = with(config, text),
                   split = NULL,
                   replace = NULL,
                   id = "row_number",
                   group = "group_number",
                   extra_cols = NULL,
                   ngrams,
                   nchar_max = 8000,
                   concepts,
                   concepts_suppl = "\\t",
                   wrap = FALSE,
                   wrap_suppl = FALSE,
                   upper_only = NA,
                   limits = "both",
                   union = "\\t\\t",
                   exclus_man = "\\t",
                   exclus_auto_except = NA,
                   highlight_weight = "bold",
                   highlight_color = "red",
                   highlight_bg = "#ffffff",
                   print = TRUE,
                   html_save = FALSE,
                   html_popup = FALSE,
                   dir_suffix = sample,
                   filename_suffix = sample,
                   seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  if (is.character(data)) data <- get(data)

  config_str <- with(config, str)

  filter <- enexpr(filter)

  concepts <-
  config$concepts |>
  with(eval(enexpr(concepts))) |>
    concepts_reduce()

  data <-
  data |>
    mutate(row_number = row_number(),
           group_number = row_number(),
           .before = everything())

  cli_h1("edstr_extract")
  cli_text("\n\n")

### SAVE PARAMS ------------------------------------------------------------------

  dir_name <- glue("{with(config, file)}_extract")
  save_dir <- glue("{with(config, dir)}/{dir_name}")
  save_files <- dir_name

  if (!is.null(dir_suffix)) save_dir <- glue("{save_dir}_{dir_suffix}")

  if (!is.null(filename_suffix)) save_files <- glue("{dir_name}_{filename_suffix}")

  if (!file.exists(glue(save_dir))) dir.create(path = glue(save_dir))

  .save_extract <- glue("{save_dir}/{save_files}")

  .lib <- glue("{save_dir}/lib")
  if (exists(.lib)) unlink(.lib, recursive = TRUE)

### FILTERS ----------------------------------------------------------------------

  data_total <- data

  if (!is.null(sample)) {

    data <- data[sample(nrow(data), sample), ]

    cli_alert_info("sample: {sample}")

    if (is.null(filter)) cli_text("\n\n")

  } else sample <- nrow(data_total)

  if (!is.null(filter)) {

    data <- data |> filter(!!filter)

    cli_alert_info("filter on")
    cli_text("\n\n")

  }

### SPLIT ------------------------------------------------------------

  cli_progress_step("{.strong Split}")

  data_split <-
  data |>
    select(id, group, extra_cols, text_input) |>
    unnest_tokens(output = !!text_input,
                  input = !!text_input,
                  token = str_split,
                  pattern = str_u(with(config_str, eval(enexpr(split)))),
                  to_lower = FALSE)

  cli_progress_done()
  cli_text("\n\n")

### REPLACE ------------------------------------------------------------

  cli_progress_step("{.strong Replace}")

  data_split <-
  data_split |>
    mutate(!!text_input :=
             reduce(with(config_str, !!enexpr(replace)),
                    str_replace_all,
                    .init = get(text_input)))

  cli_progress_done()
  cli_text("\n\n")

### LOWER, REMOVE PUNCT AND TOKENIZE ---------------------------------------------

  cli_progress_step("{.strong Tokenize}")

  data_clean <-
  data_split |>
    mutate(!!text_input :=
             get(text_input) |>
               iconv(from = "UTF-8",
                     to = "ASCII//TRANSLIT") |>
               str_replace_all("'", " "))

  .ngrams <-
  ngrams |>
    map(~ data_clean |>
          unnest_tokens(output = !!text_input,
                        input = !!text_input,
                        token = "ngrams",
                        n = .) |>
          distinct())

  if (!is.list(concepts)) concepts <- as.list(c("<concept>" = concepts))

  lim <- \(start, end) glue("{start}({str_u(concepts)}){end}")

  switch(limits,
         "start" = .cpts_str <- lim("^", ""),
         "end" = .cpts_str <- lim("", "$"),
         "both" = .cpts_str <- lim("^", "$"),
         "none" = .cpts_str <- lim("", ""))

  .cpts_names <- str_c(names(concepts), collapse = ", ")

  .split <-
  seq_along(ngrams) |>
    map(~ .ngrams[[.]] |>
          filter(str_detect(get(text_input), .cpts_str)) |>
          count(!!.cpts_names := get(text_input), sort = TRUE))

  .match <-
  map2(.x = seq_along(ngrams),
       .y = ngrams,
       ~ .ngrams[[.x]][c(group, id, text_input)] |>
         mutate(ngrams = .y) |>
         filter(get(text_input) %in% c(.split[[.]][[!!.cpts_names]])))

  data_match <- bind_rows(.match)

  if (nrow(data_match) == 0) cli_abort("No results")

  cli_progress_done()
  cli_text("\n\n")

### SET EXCLUSION LISTS -----------------------------------------------------------

  cli_progress_step("{.strong Exclusions}")

  str_exclus <-
  data_match[text_input] |>
    filter(!str_detect(!!text_input, glue("^({str_u(exclus_auto_except)})$"))) |>
    distinct() |>
    pull()

  str_exclus_bb <- glue("\\b{str_exclus}\\b")
  str_exclus_.sb <- glue(".\\s{str_exclus}\\b")

  data_exclus <-
  data_match |>
    select(all_of(text_input), ngrams) |>
    distinct()

  exclus_auto_start <-
  purrr::map2_df(.x = str_exclus_bb,
                 .y = str_exclus,
                 ~ data_exclus |>
                   filter(str_starts(get(text_input), .x)
                          & !str_ends(get(text_input), .x)) |>
                   mutate(start_match = .y))

  exclus_auto_end <-
  purrr::map2_df(.x = str_exclus_bb,
                 .y = str_exclus,
                 ~ data_exclus |>
                   filter(!str_starts(get(text_input), .x)
                          & str_ends(get(text_input), .x)) |>
                   mutate(end_match = .y))

  exclus_auto_both <-
  purrr::pmap_df(list(str_exclus_bb,
                      str_exclus_.sb,
                      str_exclus),
                 ~ data_exclus |>
                   filter(str_starts(get(text_input), ..1)
                          & str_ends(get(text_input), ..2)) |>
                   mutate(both_match = ..3))

  data_exclus_auto <-
  exclus_auto_start |>
    full_join(exclus_auto_end,
              by = c(text_input, "ngrams"),
              relationship = "many-to-many") |>
    full_join(exclus_auto_both,
              by = c(text_input, "ngrams"),
              relationship = "many-to-many")

  if (nrow(data_exclus_auto) > 0) {

    data_exclus_auto <-
    data_exclus_auto |>
      filter(!str_detect(end_match, str_u(data_exclus_auto[[text_input]]))
             | is.na(end_match)) |>
      filter(!str_detect(start_match, str_u(data_exclus_auto[[text_input]]))
             | is.na(start_match))

  }

  data_match_final <-
  data_match |>
    anti_join(data_exclus_auto[text_input],
              by = text_input)

  data_exclus_man <-
  data_match_final |>
    filter(str_detect(get(text_input), str_u(exclus_man)))

  data_match_final <-
  data_match_final |>
    anti_join(data_exclus_man[text_input],
              by = text_input)

  cli_progress_done()
  cli_text("\n\n")

### EXTRACT ----------------------------------------------------------------------

  cli_progress_step("{.strong Extract}")

  data_id <-
  names(concepts) |>
    map(~ data_match_final |>
          mutate(concept =
                   ifelse(str_starts(get(text_input), str_u(concepts[[.]])), ., NA))) |>
    bind_rows() |>
    drop_na()

  data_count <-
  data_id |>
    count(!!text_input := get(text_input), concept, ngrams,
          sort = TRUE)

  data_extract <-
  data_count[[text_input]] |>
    str_replace_all(glue("\\b{upper_only}\\b"),
                    glue("(?-i){toupper(upper_only)}"))

  data_extract <- str_c(data_extract, collapse = "\\b|(?i)\\b")
  data_extract <- glue("(?i)\\b{data_extract}\\b")

  data_extract_replace <-
  list2("(?<={union})\\s+" := "\\\\s?(-|\\\\+)?\\\\s?",
        "e" = ".",
        "\\sa\\s" = " . ") |>
    unlist()

  data_extract <-
  reduce(list(data_extract_replace),
         str_replace_all,
         .init = data_extract)

  .cpts_label <- str_unique(data_id$concept)

  .wrap <- \(x, n) {

    c(x - as.integer(n), x, x + as.integer(n))

  }

  .rows <-
  list(concepts = data_extract,
       concepts_suppl = concepts_suppl) |>
    map(~ data_clean[[text_input]] |>
          str_detect(.) |>
          which())

  .rows <-
  c(.wrap(.rows$concepts_suppl, wrap_suppl),
    .wrap(.rows$concepts, wrap))

  data_str_br_db <-
  data_split[sort(.rows), ] |>
    inner_join(data_id[names(data_id) != text_input],
               by = c(id, group),
               relationship = "many-to-many") |>
    select(all_of(c(id, group, extra_cols, text_input)),
           concept) |>
    distinct() |>
    pivot_wider(names_from = concept,
                values_from = concept) |>
    mutate(across(any_of(.cpts_label), ~ ifelse(!is.na(.), 1, NA))) |>
    group_by(!!id := get(id)) |>
    mutate(n_row = seq_along(text_input)) |>
    fill(starts_with(.cpts_label),
         .direction = "updown") |>
    mutate(!!text_input := get(text_input) |> str_flatten("<br><br>"),
           across(starts_with(.cpts_label), ~ replace_na(., 0)),
           nchar = nchar(get(text_input))) |>
    ungroup() |>
    select(-n_row) |>
    filter(!is.na(get(group))) |>
    distinct()

  data_exclus_nchar <-
  data_str_br_db[c(text_input, "nchar")] |>
    filter(nchar > nchar_max)

  data_str_br <-
  data_str_br_db |>
    group_by(!!group := get(group),
             !!text_input := get(text_input)) |>
    filter(!duplicated(get(text_input)),
           nchar <= nchar_max) |>
    ungroup()

  if (group == "group_number") data_str_br <- data_str_br |> select(-group)

  data_str_cat <-
  data_str_br |>
    mutate(!!text_input :=
             str_replace_all(get(text_input), "<br><br>", " "))

  data_str_split <-
  data_str_cat |>
    separate_rows(all_of(text_input), sep = "(?<=\\.)\\s")

  cli_progress_done()
  cli_text("\n\n")

### SUMMARIZE AND SET FINAL LIST -----------------------------------------------------------

  cli_progress_step("{.strong Summarize}")

  summary_cols <-
  list(match = data_match,
       exclus_auto = data_exclus_auto,
       exclus_man = data_exclus_man,
       keep = data_id,
       distinct = data_count)

  data_summary <-
  map2(.x = summary_cols,
       .y = names(summary_cols),
       ~ .x |>
         group_by(ngrams) |>
         count(name = .y))

  data_summary <-
  seq(summary_cols) |>
    purrr::map_df(~ tibble(ngrams = ngrams) |>
                    left_join(data_summary[[.]],
                              by = "ngrams")) |>
    group_by(ngrams) |>
    fill(names(summary_cols), .direction = "updown") |>
    distinct()

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

  assign(glue(save_files),
         data_match_list,
         envir = .GlobalEnv)

  cli_progress_done()
  cli_text("\n\n")

### PRINT ------------------------------------------------------------------------------------

  if (print) {

    print(list(ngrams = .split))

    .extract_print(concepts,
                   data_id,
                   data_count,
                   concept,
                   group)

    print(list(exclus_auto = data_exclus_auto,
               exclus_man = data_exclus_man,
               summary = data_summary))

  }

### SET OUTPUT -----------------------------------------------------------------

  if (html_popup) html_save <- TRUE

  if (html_save) {

    .extract_output(highlight_weight,
                    highlight_color,
                    highlight_bg,
                    data_str_br,
                    data_extract,
                    data_count,
                    data_match_list,
                    text_input,
                    html_popup,
                    .save_extract)

    data_match_list <-
    append(data_match_list,
           list(output = list(concepts = .data_cpts,
                              text = .data_text)))

  }

### SAVE -----------------------------------------------------------------------

  cli_progress_step("{.strong Save}")

  assign(glue(save_files),
         data_match_list,
         envir = .GlobalEnv)

  save(list = glue(save_files),
       file = glue("{.save_extract}.RData"))

  write_excel_csv(data_count, file = glue("{.save_extract}_concepts.csv"))
  write_excel_csv(data_str_cat, file = glue("{.save_extract}_text.csv"))

  cli_progress_done()

### CLI -----------------------------------------------------------------------

  .extract_cli(data,
               data_total,
               data_match,
               data_id,
               data_count,
               data_str_br,
               data_str_br_db,
               data_exclus_auto,
               data_exclus_man,
               data_exclus_nchar,
               text_input,
               nchar_max,
               id,
               group,
               save_dir)

}
