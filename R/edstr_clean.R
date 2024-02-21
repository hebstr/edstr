#' Clean a text input with patterns to replace
#'
#' @param data
#' @param sample
#' @param filter
#' @param replace
#' @param load
#' @param config
#' @param config_str
#' @param text_input
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_clean <- \(data = glue::glue("{with(config, file)}_import"),
                 sample = NULL,
                 filter = NULL,
                 replace,
                 load = FALSE,
                 config = get(.config_name),
                 config_str = with(config, str),
                 text_input = with(config, text)) {

  cli_error_config()

  if (is.character(config)) config <- get(config)

  config_dir <- with(config, dir)
  config_file <- glue::glue("{with(config, file)}_clean")
  config_save <- glue::glue("{config_dir}/{config_file}.RData")

  file_import <- glue::glue("{with(config, file)}_import")

  cli::cli_h1("edstr_clean")
  cli::cli_text("\n\n")

  if (!load) {

### INPUT ---------------------------------------------------------------------------------

    if (identical(data, file_import) & !exists(file_import)) {

      cli::cli_abort(c("{.strong {file_import}} doesn't exists!",
                       "i" = "Please create {.strong {file_import}} first with {.fn edstr_import}"))

    }

    if (is.character(data)) data <- get(data)

    data_total <- data

### FILTERS ---------------------------------------------------------------------------------

    filter <- rlang::enexpr(filter)

    if (!is.null(sample)) {

      data <- data[sample(nrow(data), sample), ]

      cli::cli_alert_info("sample: {sample}")

      if (is.null(filter)) cli::cli_text("\n\n")

    }

    if (!is.null(filter)) {

      data <- data |> dplyr::filter(!!filter)

      cli::cli_alert_info("filter ON")
      cli::cli_text("\n\n")

    }

### REPLACE ---------------------------------------------------------------------------------

    cli::cli_progress_step("Cleaning {.strong {file_import}}")

    replace <- with(config_str, eval(rlang::enexpr(replace)))

    if (!is.list(replace)) replace <- list(replace)

    data_clean <-
    data |>
      dplyr::mutate(!!text_input :=
                      purrr::reduce(replace,
                                    stringr::str_replace_all,
                                    .init = get(text_input)))

    prop_import <- nrow(data_clean) / nrow(data_total)

    if (prop_import < 1) {

      prop_import <- scales::percent(prop_import, accuracy = 0.1)
      prop_import <- glue::glue("({prop_import} of {file_import})")

    } else prop_import <- NULL

### SAVE ---------------------------------------------------------------------------------

    cli::cli_progress_step("Saving {.strong {config_file}}")

    assign(config_file, data_clean, envir = .GlobalEnv)

    save(list = config_file, file = config_save)

    cli::cli_progress_done()

### CLI ---------------------------------------------------------------------------------

    cli::cli_alert_success("{.strong {config_file}} saved to {.path {config_save}}")
    cli::cli_text("\n\n")
    cli::cli_alert_info("{.strong Dimensions}")
    cli::cli_ul()
      cli::cli_li("{nrow(data_clean)} observations {prop_import}")
      cli::cli_li("{ncol(data_clean)} variables")
      cli::cli_end()
    cli::cli_text("\n\n")
    cli::cli_rule()

  } else {

    eval(cli_load())

  }

}
