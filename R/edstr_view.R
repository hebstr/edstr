#' Title
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
                mode = c("raw", "start_word", "all_word", "sentence"),
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

    str_name <- glue::glue("_{names(str)}")

  } else str_name <- ""

  config_file <- glue::glue("{with(config, file)}_view{str_name}")

  filter <- rlang::enexpr(filter)
  mode <- rlang::arg_match(mode)

  q_right <- glue::glue("q{quantile_right*100}")
  over_q_right <- stringr::str_c("over_", q_right)

  cli::cli_h1("edstr_view")
  cli::cli_text("\n\n")

  cli::cli_progress_step("Creating {.strong {config_file}}")

  if (!is.null(filter)) data <- data |> dplyr::filter(!!filter)

### REPLACE ------------------------------------------------------------------------------

  replace <- with(config_str, eval(rlang::enexpr(replace)))

  if (!is.null(replace)) {

    if (!is.list(replace)) replace <- list(replace)

    data <-
    data |>
      dplyr::mutate(!!text_input :=
                      purrr::reduce(replace,
                                    stringr::str_replace_all,
                                    .init = get(text_input)))

  }

### MATCH ------------------------------------------------------------------------------

  enstr <- \(x, y = x) str <- glue::glue("{x}{str}{y}")

  if (!is.null(ngram_max)) {

    str <- enstr('', '(\\w*\\s*((-|/)\\s*)?\\w+)')
    str <- glue::glue("{str}{{1,", ngram_max - 1, "}}")

  }

  if (!case_sensitive) str <- enstr("(?i)", "")

  switch(mode,
         "start_word" = str <- enstr("", "\\w*"),
         "all_word" = str <- enstr("\\w*"),
         "sentence" = str <- enstr(".*"))

  data_match <-
  data |>
    dplyr::mutate(match = stringr::str_extract_all(data[[text_input]], str)) |>
    tidyr::unnest(match) |>
    dplyr::mutate(nchar = nchar(match),
                  !!q_right :=
                    stats::quantile(nchar, probs = quantile_right, na.rm = TRUE),
                  !!over_q_right :=
                    ifelse(nchar > get(q_right), 1, 0)) |>
    dplyr::select(id, text_input, match, nchar, dplyr::matches("(over_)?q(\\d+)")) |>
    dplyr::arrange(dplyr::desc(nchar)) |>
    tidyr::drop_na()

  if (nrow(data_match) == 0) cli::cli_abort("No result")

### DISTINCT ------------------------------------------------------------------------------

  data_distinct <-
  data_match |>
    dplyr::count(match,
                 nchar,
                 get(q_right),
                 get(over_q_right)) |>
    dplyr::arrange(dplyr::desc(nchar)) |>
    dplyr::mutate(n = factor(n))

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
    dplyr::distinct() |>
    tidyr::drop_na()

### PRINT ------------------------------------------------------------------------------

  data_print <-
  data_match[[text_input]][sample] |>
    unique() |>
    stringr::str_view(str, ...)

  if (raw & !is.null(filter)) {

    data_print <-
    data |>
      dplyr::filter(!!filter) |>
      dplyr::pull(text_input)

  }

  print(data_print)

### OUTPUT ------------------------------------------------------------------------------

  assign(config_file,
         list(all_matches = data_match,
              distinct_matches = data_distinct,
              sample = data_sample),
         envir = .GlobalEnv)

  median_match <-
  data_match |>
    dplyr::count(get(id), get(text_input)) |>
    dplyr::pull(n) |>
    stats::median()

  if (dplyr::n_distinct(data_match$nchar) >= 50) {

    data_plot <-
    data_match |>
      ggplot2::ggplot() +
      ggplot2::aes(nchar) +
      ggplot2::geom_density(color = "#0099DD",
                            fill = "#0099DD") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = get(q_right)),
                          color = "#BC3C33")

    print(data_plot)

  }

### CLI ------------------------------------------------------------------------------

  cli::cli_h1("edstr_view")
  cli::cli_text("\n\n")
  cli::cli_progress_done()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Details}")
  cli::cli_ul()
    if (!is.null(filter)) cli::cli_li("filter ON")
    cli::cli_li("sample: {nrow(data_sample)} {sample_max}")
    cli::cli_li("all matches: {nrow(data_match)} (~ {median_match} par {id})")
    cli::cli_li("distinct matches: {nrow(data_distinct)}")
    cli::cli_end()
  cli::cli_text("\n\n")

  data_distinct |>
    dplyr::select(distinct_matches = match, nchar, n) |>
    dplyr::arrange(dplyr::desc(n)) |>
    print()

  cli::cli_text("\n\n")
  cli::cli_rule()

}
