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
#' @param quantile_right
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
                quantile_right = .99,
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

  q_right <- glue("q{quantile_right*100}")
  over_q_right <- str_c("over_", q_right)

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

### MATCH ----------------------------------------------------------------------

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
    mutate(nchar = nchar(match),
           !!q_right := quantile(nchar, probs = quantile_right, na.rm = TRUE),
           !!over_q_right := ifelse(nchar > get(q_right), 1, 0)) |>
    select(id, text_input, match, nchar, matches("(over_)?q(\\d+)")) |>
    arrange(desc(nchar)) |>
    drop_na()

  if (nrow(data_match) == 0) cli_abort("{.strong No match}")

### COUNT ----------------------------------------------------------------------

  data_count <-
  data_match |>
    count(match,
          nchar,
          !!q_right := get(q_right),
          !!over_q_right := get(over_q_right),
          sort = TRUE)

### SET LIST -------------------------------------------------------------------

  data_list <-
  list(match = data_match,
       count = data_count)

  if (!quiet) {

### ASSIGN ---------------------------------------------------------------------

    assign(config_file,
           data_list,
           envir = .GlobalEnv)

### OUTPUT PLOT -----------------------------------------------------------------

    if (n_distinct(data_match$nchar) >= 50) {

      data_output_plot <-
      ggplot(data_match) +
        aes(nchar) +
        geom_density(color = "#0099EE",
                     fill = "#0099EE") +
        geom_vline(mapping = aes(xintercept = get(q_right)),
                   color = "#CC0C00")

      print(data_output_plot)

    }

### OUTPUT SAMPLE ---------------------------------------------------------------

  if (nrow(data_match) > output_sample) {

    output_sample <- sample(nrow(data_match), output_sample)
    output_sample_max <- "(max)"

  } else {

    output_sample <- 1:nrow(data_match)
    output_sample_max <- NULL

  }

    data_output_sample <-
    data_match[[text_input]][output_sample] |>
      unique() |>
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

    cli_n_match <- n_distinct(data_match[[id]])
    cli_p_match <- label_percent(0.1)(cli_n_match / nrow(data))

    cli_alert_info("{.strong Documents:} {nrow(data)} {id}")
    cli_text("\n\n")

    cli_alert_info("{.strong Matching}")
    cli_ul()
      cli_li("Total: {nrow(data_match)} from {cli_n_match} {id} ({cli_p_match} {id})")
      cli_li("Distinct: {nrow(data_count)}")
      cli_end()

    cli_text("\n\n")
    cli_rule()
    cli_text("\n\n")

  } else {

    regex_match <- data_list$count[c("match", "n")]

    return(regex_match)

  }

}
