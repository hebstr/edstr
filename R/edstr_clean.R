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
edstr_clean <- \(data = glue("{with(config, file)}_import"),
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
  config_file <- glue("{with(config, file)}_clean")
  config_save <- glue("{config_dir}/{config_file}.RData")

  file_import <- glue("{with(config, file)}_import")

  cli_h1("edstr_clean")
  cli_text("\n\n")

  if (!load) {

### INPUT ---------------------------------------------------------------------------------

    if (identical(data, file_import) & !exists(file_import)) {

      cli_abort(c("{.strong {file_import}} doesn't exists!",
                  "i" = "Please create {.strong {file_import}} first with {.fn edstr_import}"))

    }

    if (is.character(data)) data <- get(data)

    data_total <- data

### FILTERS ---------------------------------------------------------------------------------

    filter <- enexpr(filter)

    if (!is.null(sample)) {

      data <- data[sample(nrow(data), sample), ]

      cli_alert_info("sample: {sample}")

      if (is.null(filter)) cli_text("\n\n")

    }

    if (!is.null(filter)) {

      data <- data |> filter(!!filter)

      cli_alert_info("filter ON")
      cli_text("\n\n")

    }

### REPLACE ---------------------------------------------------------------------------------

    cli_progress_step("Cleaning {.strong {file_import}}")

    replace <- with(config_str, eval(enexpr(replace)))

    if (!is.list(replace)) replace <- list(replace)

    data_clean <-
    data |>
      mutate(!!text_input :=
               reduce(replace,
                      str_replace_all,
                      .init = get(text_input)))

    prop_import <- nrow(data_clean) / nrow(data_total)

    if (prop_import < 1) {

      prop_import <- scales::percent(prop_import, accuracy = 0.1)
      prop_import <- glue("({prop_import} of {file_import})")

    } else prop_import <- NULL

    cli_progress_done()

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
