.clean_save <- \(
  data,
  text,
  replace,
  config
) {

  check_class(data, "data.frame")
  check_class(text, "character")

  cli_h1("edstr_clean"); br()

  cli_progress_step("Nettoyage du texte ({.strong {text}})")

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

.clean_load <- \(config) {

  cli_h1("edstr_clean"); br()

  cli_load(
    dir = config$dir,
    file = config$file,
    save = config$save
  )

}

#' Nettoyer les données textuelles
#'
#' @param data data
#' @param text text
#' @param replace replace
#'
#' @returns value
#' @export
#'
#' @examples "example"
#'
edstr_clean <- \(
  data,
  text = getOption("edstr_text"),
  replace
) {

  config <- check_config("clean")

  fun_save <- \() .clean_save(data, text, replace, config)

  if (fs::file_exists(config$save)) {

    cli_check(
      config_file = config$file,
      fun_save = fun_save,
      fun_load = \() .clean_load(config)
    )

  } else {

    fun_save()

  }

}
