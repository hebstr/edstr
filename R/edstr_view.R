#' Explorer
#'
#' @param data
#' @param text_input
#' @param sample
#' @param filter
#' @param replace
#' @param str
#' @param case_sensitive
#' @param mode
#' @param ngram_max
#' @param id
#' @param quantile_right
#' @param raw
#' @param config
#' @param config_str
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_view <- \(data,
                sample = 10,
                filter = NULL,
                replace = NULL,
                str = NULL,
                case_sensitive = FALSE,
                mode = c("raw", "start_word", "full_word", "sentence"),
                ngram_max = NULL,
                id = NULL,
                quantile_right = .99,
                raw = FALSE,
                config = get(.config_name),
                config_str = with(config, str),
                text_input = with(config, text),
                ...) {

  if (is.character(data)) data <- get(data)
  if (is.character(config)) config <- get(config)

  if (!is.null(names(str))) {

    str_name <- glue("_{names(str)}")

  } else str_name <- ""

  config_file <- glue("{with(config, file)}_view{str_name}")

  mode <- arg_match(mode)

  q_right <- glue("q{quantile_right*100}")
  over_q_right <- str_c("over_", q_right)

  cli_h1("edstr_view")
  cli_text("\n\n")

  cli_progress_step("Creating {.strong {config_file}}")

  filter <- enexpr(filter)

  if (!is.null(filter)) data <- filter(data, !!filter)

### REPLACE ------------------------------------------------------------------------------

  if (!is.null(replace)) {

    if (!is.list(replace)) replace <- list(replace)

    data <-
    data |>
      mutate(!!text_input :=
               reduce(replace,
                      str_replace_all,
                      .init = get(text_input)))

  }

### MATCH ------------------------------------------------------------------------------

  enstr <- \(x, y = x) str <- glue("{x}{str}{y}")

  if (!is.null(ngram_max)) {

    str <- enstr('', '(\\w*\\s*((-|/)\\s*)?\\w+)')
    str <- glue("{str}{{1,", ngram_max - 1, "}}")

  }

  if (!case_sensitive) str <- enstr("(?i)", "")

  switch(mode,
         "start_word" = str <- enstr("", "\\w*"),
         "full_word" = str <- enstr("\\w*"),
         "sentence" = str <- enstr(".*"))

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

### DISTINCT ------------------------------------------------------------------------------

  data_distinct <-
  data_match |>
    count(match,
          nchar,
          !!q_right := get(q_right),
          !!over_q_right := get(over_q_right),
          sort = TRUE)

### SAMPLE ------------------------------------------------------------------------------

  if (nrow(data_match) > sample) {

    sample <- sample(nrow(data_match), sample)
    sample_max <- "(max)"

  } else {

    sample <- 1:nrow(data_match)
    sample_max <- NULL

  }

  data_sample <-
  data_match[sample, ] |>
    distinct() |>
    drop_na()

### PRINT ------------------------------------------------------------------------------

  data_print <-
  data_match[[text_input]][sample] |>
    unique() |>
    str_view(str, ...)

  if (raw & !is.null(filter)) {

    data_print <-
    data |>
      filter(!!filter) |>
      pull(text_input)

  }

  print(data_print)

### OUTPUT ------------------------------------------------------------------------------

  assign(config_file,
         list(match = data_match,
              distinct = data_distinct,
              sample = data_sample),
         envir = .GlobalEnv)

  if (n_distinct(data_match$nchar) >= 50) {

    data_plot <-
    ggplot(data_match) +
      aes(nchar) +
      geom_density(color = "darkcyan",
                   fill = "darkcyan") +
      geom_vline(mapping = aes(xintercept = get(q_right)),
                 color = "darkred")

    print(data_plot)

  }

### CLI ------------------------------------------------------------------------------

  median_match <-
  data_match |>
    count(get(id), get(text_input)) |>
    pull(n) |>
    median() |>
    round(1)

  cli_h1("edstr_view")
  cli_text("\n\n")
  cli_progress_done()
  cli_text("\n\n")
  cli_alert_info("{.strong Details}")
  cli_ul()
    cli_li("sample: {nrow(data_sample)} {sample_max}")
    cli_li("total matching: {nrow(data_match)} (median: {median_match} by {id})")
    cli_li("distinct matching: {nrow(data_distinct)}")
    cli_end()
  cli_text("\n\n")

  data_distinct |>
    select(distinct = match, nchar, n) |>
    arrange(desc(n)) |>
    print()

  cli_text("\n\n")
  cli_rule()

}
