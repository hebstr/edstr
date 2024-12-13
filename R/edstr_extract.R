#' Title
#'
#' @param data
#' @param sample
#' @param seed
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
#' @param exclus_manual
#' @param exclus_auto_except
#' @param highlight_weight
#' @param highlight_color
#' @param highlight_bg
#' @param print
#' @param html_save
#' @param html_popup
#' @param dir_suffix
#' @param filename_suffix
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_extract <- \(data = glue("{with(config, file)}_clean"),
                   sample = NULL,
                   seed = NULL,
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
                   exclus_manual = NULL,
                   exclus_auto_except = NULL,
                   highlight_weight = "bold",
                   highlight_color = "red",
                   highlight_bg = "#ffffff",
                   print = TRUE,
                   html_save = FALSE,
                   html_popup = FALSE,
                   dir_suffix = sample,
                   filename_suffix = sample) {

  if (!is.null(seed)) set.seed(seed)

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  if (is.character(data)) data <- get(data)

  config_str <- with(config, str)

  filter <- enexpr(filter)

  if (!is.list(concepts)) concepts <- list("<concept>" = concepts)

  concepts <-
  config$concepts |>
    with(eval(enexpr(concepts))) |>
    concepts_reduce()

  data <-
  c("group_number", "row_number") |>
    reduce(rownames_to_column, .init = data)

  cli_h1("edstr_extract")
  cli_text("\n\n")

### SAVE PARAMS ----------------------------------------------------------------

  dir_name <- glue("{with(config, file)}_extract")
  save_dir <- glue("{with(config, dir)}/{dir_name}")
  save_files <- dir_name

  if (!is.null(dir_suffix)) save_dir <- glue("{save_dir}_{dir_suffix}")

  if (!is.null(filename_suffix)) save_files <- glue("{dir_name}_{filename_suffix}")

  if (!file.exists(glue(save_dir))) dir.create(path = glue(save_dir))

  .save_extract <- glue("{save_dir}/{save_files}")

  .lib <- glue("{save_dir}/lib")
  if (exists(.lib)) unlink(.lib, recursive = TRUE)

### FILTERS --------------------------------------------------------------------

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

### SPLIT ----------------------------------------------------------------------

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

### REPLACE --------------------------------------------------------------------

  cli_progress_step("{.strong Replace}")

  data_split <-
  data_split |>
    mutate(!!text_input :=
             reduce(with(config_str, !!enexpr(replace)),
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

  lim <- \(x, y) glue("{x}({str_u(concepts)}){y}")

  switch(limits,
         "start" = .cpts_str <- lim("^", ""),
         "end" = .cpts_str <- lim("", "$"),
         "both" = .cpts_str <- lim("^", "$"),
         "none" = .cpts_str <- lim("", ""))

  data_ngrams_match <-
  ngrams |>
    imap(~ data_ngrams[[.y]] |>
           filter(str_detect(get(text_input), .cpts_str)) |>
           count(pick(text_input), sort = TRUE)) |>
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

  cli_progress_done()
  cli_text("\n\n")

### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusions}")


  if (!is.null(exclus_auto_except)) {

    data_match <- filter(data_match, !str_detect(get(text_input), exclus_auto_except))

  }

  auto_exclude <- \(regex, name) {

    data_match[[text_input]] |>
      unique() |>
      map(~ data_match |>
            distinct(ngrams, concept, pick(text_input)) |>
            filter(str_detect(get(text_input), glue(regex))) |>
            mutate(!!name := .)) |>
      list_rbind()

  }

  data_exclus <-
  list(auto =
         list(start = "^{.}\\s",
              end = "\\s{.}$",
              start_end = "{.}.+{.}$") |>
         imap(auto_exclude) |>
         reduce(full_join, by = c("ngrams", "concept", text_input)),
       manual =
         data_match |>
           distinct(ngrams, concept, pick(text_input)) |>
           filter(str_detect(get(text_input), exclus_manual %||% NA_character_)))

  # if (nrow(data_exclus$auto) > 0) {
  #
  # # data_exclus$auto <-
  # # data_exclus$auto |>
  # #   filter(!str_detect(end, str_u(text_input)) | is.na(end)) |>
  # #   filter(!str_detect(start, str_u(text_input)) | is.na(start))
  #
  # }

  data_match_final <-
  base::split(data_match,
              ~ if_else(get(text_input) %in% bind_rows(data_exclus)[[text_input]],
                        true = "drop",
                        false = "keep"))

  cli_progress_done()
  cli_text("\n\n")

### EXTRACT --------------------------------------------------------------------

  cli_progress_step("{.strong Extract}")

  data_id <- data_match_final$keep

  data_count <-
  list2(match = data_id,
        !!id := data_id |> distinct(pick(-group)),
        !!group := data_id |> distinct(pick(-id))) |>
    imap(~ . |>
           count(concept, pick(text_input), ngrams,
                 name = .y,
                 sort = TRUE)) |>
    reduce(left_join, by = c("concept", text_input, "ngrams"))




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

### SUMMARIZE ------------------------------------------------------------------

  cli_progress_step("{.strong Summarize}")

  data_summary <-
  list(match = data_match,
       exclus_auto = data_exclus$auto,
       exclus_manual = data_exclus$manual,
       keep = data_id,
       distinct = data_count) |>
    imap(~ .x |> count(ngrams, name = .y)) |>
    reduce(full_join, by = "ngrams")

  data_match_list <-
  list(data = data,
       split = data_split,
       match = data_match,
       id = data_id,
       count = data_count,
       regex = data_extract,
       str =
         list(br = data_str_br,
              cat = data_str_cat,
              split = data_str_split),
       exclusions =
         list(auto = data_exclus$auto,
              manual = data_exclus$manual,
              nchar = data_exclus_nchar),
       summary = data_summary)

  assign(glue(save_files),
         data_match_list,
         envir = .GlobalEnv)

  cli_progress_done()
  cli_text("\n\n")

### PRINT ------------------------------------------------------------------------------------

  if (print) {

    print(list(ngrams = data_ngrams_match))

    .extract_print(concepts,
                   data_id,
                   data_count,
                   concept,
                   group)

    print(list(exclus = data_exclus,
               summary = data_summary,
               count = data_count))

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
               data_exclus$auto,
               data_exclus$manual,
               data_exclus_nchar,
               text_input,
               nchar_max,
               id,
               group,
               save_dir)

}
