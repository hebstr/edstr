#' Explorer
#'
#' @param data
#' @param config
#' @param config_str
#' @param text_input
#' @param filter
#' @param replace
#' @param str
#' @param case_sensitive
#' @param limits
#' @param ngram_max
#' @param id
#' @param raw
#' @param output_sample
#' @param quiet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_view <- \(data,
                config = get(.config_name),
                config_str = with(config, str),
                text_input = with(config, text),
                filter = NULL,
                replace = NULL,
                str = NULL,
                case_sensitive = FALSE,
                limits = c("asis", "start-end", "start", "end", "sentence"),
                ngram_max = NULL,
                id = NULL,
                raw = FALSE,
                output_sample = 5,
                quiet = FALSE,
                ...) {

  if (is.character(data)) data <- get(data)
  if (is.character(config)) config <- get(config)

  if (!is.null(names(str))) {

    str_name <- glue("_{names(str)}")

  } else str_name <- ""

  config_file <- glue("{with(config, file)}_view{str_name}")

  limits <- arg_match(limits)

  if (!quiet) {

    cli_h1("edstr_view")
    cli_text("\n\n")

    cli_progress_step("Creating {.strong {config_file}}")

  }

  filter <- enexpr(filter)

  if (!is.null(filter)) data <- filter(data, !!filter)

### REPLACE --------------------------------------------------------------------

  if (!is.null(replace)) {

    if (!is.list(replace)) replace <- list(replace)

    data <-
    data |>
      mutate(!!text_input :=
               reduce(replace,
                      str_replace_all,
                      .init = get(text_input)))

  }

### EXTRACT --------------------------------------------------------------------

  enstr <- \(x, y = x) str <- glue("{x}{str}{y}")

  if (!is.null(ngram_max)) {

    str <- enstr('', '(\\w*\\s*((-|/)\\s*)?\\w+)')
    str <- glue("{str}{{1,", ngram_max - 1, "}}")

  }

  if (!case_sensitive) str <- enstr("(?i)", "")

  switch(limits,
         "start-end" = str <- enstr("\\w*"),
         "start" = str <- enstr("", "\\w*"),
         "end" = str <- enstr("\\w*", ""),
         "sentence" = str <- enstr("(?<=<p>).*", ".*(?=</p>)"))

  data_match <-
  data |>
    mutate(match = str_extract_all(data[[text_input]], str)) |>
    unnest(match) |>
    select(id, match)

  match_id <- n_distinct(data_match[[id]])

  if (nrow(data_match) == 0) cli_abort("{.strong Aucune correspondance}")

  data_count <- data_match |> count(match, sort = TRUE)

  if (!quiet) {

### ASSIGN ---------------------------------------------------------------------

    assign(config_file,
           list(match = data_match,
                count = data_count),
           envir = .GlobalEnv)

# ### OUTPUT PLOT --------------------------------------------------------------
#
#     if (n_distinct(data_match$nchar) >= 50) {
#
#       data_output_plot <-
#       data_match |>
#         ggplot(aes(nchar)) +
#         geom_histogram()
#
#       print(data_output_plot)
#
#     }

### OUTPUT SAMPLE ---------------------------------------------------------------

    output_sample_max <- if (match_id > output_sample) "(max)" else NULL

    data_output_sample <-
    data |>
      filter(get(id) %in% data_match[[id]]) |>
      slice(1:output_sample) |>
      pull(text_input) |>
      str_view(str, ...)

    if (raw & !is.null(filter)) {

      data_output_sample <-
      data |>
        filter(!!filter) |>
        pull(text_input)

    }

    print(list(sample = data_output_sample,
               count = data_count))

    cli_progress_done()

### CLI ------------------------------------------------------------------------

    cli_h1("edstr_view")
    cli_text("\n\n")

    cli_p_match <- label_percent(0.1)(match_id / nrow(data))

    cli_alert_info("{.strong Documents :} {nrow(data)} {id}")
    cli_text("\n\n")

    cli_alert_info("{.strong Correspondances}")
    cli_ul()
      cli_li("Totales : {nrow(data_match)} parmi {match_id} {id} ({cli_p_match} {id})")
      cli_li("Distinctes : {nrow(data_count)}")
      cli_end()

    cli_text("\n\n")
    cli_rule()
    cli_text("\n\n")

  } else {

    return(data_match)

  }

}
