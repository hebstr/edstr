#' clean
#'
#' @param data data
#' @param sample sample
#' @param filter filter
#' @param text_input text_input
#' @param replace replace
#' @param load load
#'
#' @returns value
#' @export
#'
#' @examples example
edstr_clean <- \(
  data,
  sample = NULL,
  filter = NULL,
  text_input = with(config, text),
  replace,
  load = FALSE
) {

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- base::get(.config_name)

  config_dir <- config$dir
  config_file <- glue("{with(config, file)}_clean")
  config_save <- glue("{config_dir}/{config_file}.rds")

  # file_import <- glue("{with(config, file)}_import")

  cli_h1("edstr_clean")
  cli_text("\n\n")

  if (!load) {

### INPUT ----------------------------------------------------------------------

    # if (identical(data, file_import) && !exists(file_import)) {

    #   cli_error_data(data = file_import, fun = "import")

    # }

    # if (is.character(data)) data <- base::get(data)

    # data_total <- data

### FILTERS --------------------------------------------------------------------

    if (!is.null(sample)) data <- data[sample(nrow(data), sample), ]

    filter <- enexpr(filter)

    if (!is.null(filter)) data <- filter(data, !!filter)

### CLEAN ----------------------------------------------------------------------

    cli_progress_step("Nettoyage du fichier (???)")

    replace <- base::get(load(replace))

    if (!is.list(replace)) replace <- list(replace)

    data_clean <- mutate(
      .data = data,
      !!text_input := reduce(
        .x = replace,
        .f = str_replace_all,
        .init = .data[[text_input]]
      )
    )

    cli_progress_done()

### CLI ------------------------------------------------------------------------

    cli_save(
      data = data_clean,
      config_file = config_file,
      config_save = config_save
    )

  } else {

    cli_load(
      dir = config_dir,
      file = config_file,
      save = config_save
    )

  }

}
