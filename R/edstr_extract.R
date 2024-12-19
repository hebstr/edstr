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
                   text_input = with(config, text),
                   split = with(config_str, split),
                   replace = with(config_str, replace),
                   sample = NULL,
                   seed = NULL,
                   filter = NULL,
                   id = "",
                   group = "",
                   ngrams,
                   nchar_max = 8000,
                   concepts,
                   wrap = FALSE,
                   limits = "both",
                   exclus_manual = NULL,
                   exclus_auto_except = NULL,
                   regex_replace = c("[aeo]" = "\\\\w", "\\s" = "."),
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

  config_str <- with(config, str)

  if (!is.list(concepts)) concepts <- list("<concept>" = concepts)

  concepts <-
  config$concepts |>
    with(eval(enexpr(concepts))) |>
    concepts_reduce()

  if (is.character(data)) data <- get(data)

  if (!id %in% names(data)) id <- "id_number"

  if (!group %in% names(data)) group <- "group_number"

  data <-
  reduce(c("group_number", "id_number"),
         rownames_to_column,
         .init = data)

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

  lim <- \(start, end) glue("{start}({str_u(concepts)}){end}")

  switch(limits,
         "start" = .cpts_str <- lim("^", ""),
         "end" = .cpts_str <- lim("", "$"),
         "both" = .cpts_str <- lim("^", "$"),
         "none" = .cpts_str <- lim("", ""))

  data_ngrams_match <-
  ngrams |>
    imap(~ data_ngrams[[.y]] |>
           filter(str_detect(get(text_input), .cpts_str)) |>
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

  data_regex <-
  reduce(list(regex_replace),
         str_replace_all,
         .init =
           glue("(?i)\\b{data_count[[text_input]]}\\b") |>
             paste(collapse = "|"))

  set_wrap <- \(x) if (wrap) c(x - 1, x, x + 1) else x

  .which_rows <-
  data_clean[[text_input]] |>
    str_detect(data_regex) |>
    which() |>
    set_wrap()

  data_str <-
  list(data = data |> select(-text_input),
       concept =
         data_id |>
           distinct(pick(id, group), concept) |>
           pivot_wider(names_from = concept,
                       values_from = concept) |>
           mutate(across(any_of(names(concepts)), ~ ifelse(!is.na(.), 1, 0))),
       extract =
         data_id |>
           distinct(pick(id, group, text_input)) |>
           mutate(extract = str_flatten(get(text_input), " ; "),
                  .by = id,
                  .keep = "unused") |>
           distinct(),
       text =
         data_split[.which_rows, ] |>
           mutate(!!text_input := str_flatten(get(text_input), "<br><br>"),
                  .by = id) |>
           distinct()) |>
    reduce(left_join, by = c(id, group)) |>
    mutate(nchar = nchar(get(text_input)))

  data_exclus_nchar <- data_str |> filter(nchar > nchar_max)

  data_str <- data_str |> filter(nchar <= nchar_max)

  if (!group %in% names(data)) data_str <- data_str |> select(-group)

  cli_progress_done()
  cli_text("\n\n")

### SUMMARIZE ------------------------------------------------------------------

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

  data_match_list <-
  list(split = data_split[.which_rows, ],
       regex =
         list(init =
                tibble(concept = names(concepts),
                       regex = map_chr(concepts, ~ glue("^({.})$"))),
              final = data_regex),
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
         data_total[c(id, group, text_input)] |>
           filter(!get(id) %in% data_str[[id]]),
       summary = data_summary,
       data =
         lst(raw = data_str,
             output = raw |> select(id, group, names(concepts), extract, text)))

### SET OUTPUT -----------------------------------------------------------------

  if (html_popup) html_save <- TRUE

  if (html_save) {

    .extract_output(highlight_weight,
                    highlight_color,
                    highlight_bg,
                    data_regex,
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

  cli_progress_done()
  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

### PRINT ------------------------------------------------------------------------------------

  if (print) {

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

    .print <-
    list(regex =
           tibble(concept = names(concepts),
                  regex = map_chr(concepts, ~ glue("^({.})$"))),
         ngrams = data_ngrams_match,
         count =
           list(total = data_ngrams_count,
                ngrams = data_summary$ngrams,
                concepts = data_summary$concept,
                exclus = data_count_exclus,
                final = data_count))

    print(.print)

    cli_text("\n\n")
    cli_rule()
    cli_text("\n\n")

  }

### CLI -----------------------------------------------------------------------

  cli_p_final <- label_percent(0.1)(nrow(data) / nrow(data_total))
  cli_n_join <- n_distinct(data_match[[id]])
  cli_p_join <- label_percent(0.1)(cli_n_join / nrow(data))
  cli_n_distinct_match <- n_distinct(data_match[[text_input]])
  cli_n_extract <- length(data_str[[id]])
  cli_p_extract <- label_percent(0.1)(cli_n_extract / nrow(data))
  cli_n_concept <- n_distinct(data_count$concept)
  cli_n_group <- n_distinct(data_str[[group]])
  cli_p_group <- label_percent(0.1)(cli_n_group / n_distinct(data[[group]]))

  cli_alert_info("{.strong Observations}")
  cli_ul()
  cli_ul()
    cli_li("Total: {nrow(data_total)} {id}")
    if (!is.null(sample)) cli_li("Sample: {nrow(data)} {id} ({cli_p_final})")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Matching}")
  cli_ul()
    cli_li("Total: {nrow(data_match)} from {cli_n_join} {id} ({cli_p_join} {id})")
    cli_li("Distinct: {cli_n_distinct_match}")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Exclusions}")
  cli_ul()
    cli_li("Auto: {nrow(data_match_exclus$auto)}")
    cli_li("Manual: {nrow(data_match_exclus$manual)}")
    cli_li("Over character limit (n={nchar_max}): {nrow(data_exclus_nchar)}")
    cli_end()

  # cli_text("\n\n")
  # cli_alert_info("{.strong Final matching}")
  # cli_ul()
  #   cli_li("Total: {nrow(data_match)} from {cli_n_join} {id} ({cli_p_join} {id})")
  #   cli_li("Distinct: {cli_n_distinct_match}")
  #   cli_end()

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {nrow(data_id)} matchs from}")
  cli_ul()
    cli_li("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (!is.null(group)) cli_li("{cli_n_group} {group} ({cli_p_group} {group})")
    cli_end()

  cli_text("\n\n")
  cli_alert_success("{.strong {nrow(data_count)} distinct expressions for {cli_n_concept} concepts}")

  cli_text("\n\n")
  cli_alert_success("files saved to {.path {save_dir}}")

  cli_text("\n\n")
  cli_rule()

}
