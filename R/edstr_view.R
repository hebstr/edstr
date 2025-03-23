#' Explorer
#'
#' @param data data
#' @param config config
#' @param text_input text_input
#' @param filter filter
#' @param replace replace
#' @param str str
#' @param case_sensitive case_sensitive
#' @param starts_with_only starts_with_only
#' @param ngram_max ngram_max
#' @param id id
#' @param raw raw
#' @param output_sample output_sample
#' @param quiet quiet
#' @param ... ...
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_view <- \(data,
                config = get(.config_name),
                text_input = with(config, text),
                filter = NULL,
                replace = NULL,
                str = NULL,
                case_sensitive = FALSE,
                starts_with_only = FALSE,
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

  if (starts_with_only) str <- enstr("", "\\S*")

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

    if (output_sample > 0) {

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

    } else {

      data_output_sample <- NULL

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
