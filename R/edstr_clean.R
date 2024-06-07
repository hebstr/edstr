#' Nettoyer des chaines de caractères par remplacement de patterns
#'
#' @param data
#' @param sample
#' @param filter
#' @param text_input
#' @param replace
#' @param load
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_clean <- \(data = glue::glue("{with(config, file)}_import"),
                 sample = NULL,
                 filter = NULL,
                 text_input = with(config, text),
                 replace,
                 load = FALSE) {

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  config_str <- with(config, str)
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

    cli::cli_progress_done()

### CLI ---------------------------------------------------------------------------------

    cli_save(data_clean,
             config_file,
             config_save,
             prop_import)

  } else {

    cli_load(config_dir,
             config_file,
             config_save)

  }

}
