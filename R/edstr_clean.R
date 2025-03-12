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
                 replace = with(config, replace),
                 load = FALSE) {

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  config_dir <- config$dir
  config_file <- glue("{with(config, file)}_clean")
  config_save <- glue("{config_dir}/{config_file}.RData")

  file_import <- glue("{with(config, file)}_import")

  cli_h1("edstr_clean")
  cli_text("\n\n")

  if (!load) {

### INPUT ----------------------------------------------------------------------

    if (identical(data, file_import) && !exists(file_import)) {

      cli_error_data(data = file_import,
                     fun = "import")

    }

    if (is.character(data)) data <- get(data)

    data_total <- data

### FILTERS --------------------------------------------------------------------

    if (!is.null(sample)) data <- data[sample(nrow(data), sample), ]

    filter <- enexpr(filter)

    if (!is.null(filter)) data <- filter(data, !!filter)

### CLEAN ----------------------------------------------------------------------

    cli_progress_step("Nettoyage du fichier {.strong {file_import}}")

    if (!is.list(replace)) replace <- list(replace)

    data_clean <-
    data |>
      mutate(!!text_input :=
               reduce(replace,
                      str_replace_all,
                      .init = get(text_input)))

    cli_progress_done()

### CLI ------------------------------------------------------------------------

    cli_save(data_clean,
             config_file,
             config_save)

  } else {

    cli_load(dir = config_dir,
             file = config_file,
             save = config_save)

  }

  invisible(gc())

}
