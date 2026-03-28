.clean_save <- \(
  data,
  text,
  replace,
  config
) {

  check_class(data, "data.frame")
  check_class(text, "character")
  check_replace(replace)

  if (!(text %in% names(data))) {
    cli_abort(
      "{.arg text}: column {.strong {text}} not found in {.arg data}",
      call = rlang::caller_env()
    )
  }

  cli_h1("edstr_clean"); br()

  cli_progress_step("Cleaning text ({.strong {text}})")

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

#' Clean text data
#'
#' Apply regex-based replacements to a text column and save the result as an
#' RDS file. If a cached file already exists, behaviour depends on the
#' `edstr_overwrite` option (see [edstr_config()]).
#'
#' Requires [edstr_config()] to be called first.
#'
#' @param data `<data.frame>` The data to clean. Must contain the column
#'   specified by `text`.
#' @param text `<character(1)>` Name of the text column to clean. Defaults
#'   to the `edstr_text` option set by [edstr_config()].
#' @param replace A named character vector or a list of named character
#'   vectors. Names are regex patterns, values are replacements. When a list
#'   is provided, each element is applied sequentially via
#'   [stringr::str_replace_all()].
#'
#' @return A [data.frame] with the cleaned text column.
#' @export
#'
#' @examples
#' \donttest{
#' edstr_config(
#'   edstr_dirname = tempdir(), edstr_filename = "my_study",
#'   edstr_text = "note_text", edstr_overwrite = TRUE
#' )
#'
#' df_import <- data.frame(
#'   id = 1:3,
#'   note_text = c("diabete  type\n2", "bilan normal", "diabete gestationnel")
#' )
#'
#' df_clean <- edstr_clean(
#'   data = df_import,
#'   replace = c("\\s+" = " ", "\\n" = " ")
#' )
#' }
#'
edstr_clean <- \(
  data,
  text = getOption("edstr_text"),
  replace
) {

  if (is.null(text)) {
    cli_abort(c(
      "{.arg text} is not set",
      "i" = "Set {.code edstr_text} via {.fn edstr_config} or pass {.arg text} explicitly"
    ))
  }

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
