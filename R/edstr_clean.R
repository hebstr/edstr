.clean_save <- \(
  data,
  text,
  replace,
  sample = NULL
) {

  config <- check_config("clean")

  check_class(data, "data.frame")

  cli_h1("edstr_clean")
  cli_text("\n\n")

  if (!is.null(sample)) data <- data[sample(nrow(data), sample), ]

  cli_progress_step("Nettoyage du fichier (???)")

  data_clean <- easy_replace(
    data = data,
    pattern = replace,
    text = text
  )

  cli_progress_done()

  cli_save(
    data = data_clean,
    config_file = config$file,
    config_save = config$save
  )

}

.clean_load <- \() {

  config <- check_config("clean")

  cli_h1("edstr_clean")
  cli_text("\n\n")

  cli_load(
    dir = config$dir,
    file = config$file,
    save = config$save
  )

}


#' clean
#'
#' @param data data
#' @param sample sample
#' @param text text
#' @param replace replace
#'
#' @returns value
#' @export
#'
#' @examples example
edstr_clean <- \(
  data,
  text = getOption("edstr_text"),
  replace,
  sample = NULL
) {

  config <- check_config("clean")

  if (!fs::file_exists(config$save)) {

    .clean_save(
      data = data,
      text = text,
      replace = replace,
      sample = sample
    )

  } else {

    cli_check(
      config_file = config$file,
      fun_save = .clean_save(
        data = data,
        text = text,
        replace = replace,
        sample = sample
      ),
      fun_load = .clean_load()
    )

  }

}
