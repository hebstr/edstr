#' Nettoyer des chaines de caractères par remplacement de patterns
#'
#' @param data
#' @param sample
#' @param filter
#' @param text_input
#' @param clean
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
                 clean = with(config_str, clean),
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

### INPUT ----------------------------------------------------------------------

    if (identical(data, file_import) & !exists(file_import)) {

      cli_abort(c("{.strong {file_import}} doesn't exists!",
                  "i" = "Please create {.strong {file_import}} first with {.fn edstr_import}"))

    }

    if (is.character(data)) data <- get(data)

    data_total <- data

### FILTERS --------------------------------------------------------------------

    if (!is.null(sample)) data <- data[sample(nrow(data), sample), ]

    filter <- enexpr(filter)

    if (!is.null(filter)) data <- filter(data, !!filter)

### CLEAN ----------------------------------------------------------------------

    cli_progress_step("Cleaning {.strong {file_import}}")

    if (!is.list(clean)) clean <- list(clean)

    data_clean <-
    data |>
      mutate(!!text_input :=
               reduce(clean,
                      str_replace_all,
                      .init = get(text_input)))

    prop_import <- nrow(data_clean) / nrow(data_total)

    if (prop_import < 1) {

      prop_import <- label_percent(0.1)(prop_import)
      prop_import <- glue("({prop_import} of {file_import})")

    } else prop_import <- NULL

    cli_progress_done()

### CLI ------------------------------------------------------------------------

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
