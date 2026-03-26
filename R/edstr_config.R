#' Configure the edstr pipeline
#'
#' Set global options used by all downstream functions (`edstr_import()`,
#' `edstr_clean()`, `edstr_extract()`, `edstr_view()`). Creates the output
#' directory if it does not exist. `edstr_dirname` and `edstr_filename`
#' support [glue syntax][stringr::str_glue] (e.g. `"{Sys.Date()}"`).
#'
#' @param edstr_dirname `<character(1)>` Path to the output directory.
#'   Created via [fs::dir_create()] if it does not exist.
#' @param edstr_filename `<character(1)>` Prefix used to name output files
#'   (e.g. `"my_study"` produces `my_study_import.rds`, `my_study_clean.rds`,
#'   etc.). Supports [glue syntax][stringr::str_glue].
#' @param edstr_text `<character(1)>` Name of the text column in the data.
#'   Used as the default `text` argument in `edstr_clean()` and `edstr_view()`.
#' @param edstr_overwrite `<logical(1)>` Controls caching behaviour when an
#'   output file already exists:
#'   - `TRUE`: overwrite without prompting.
#'   - `FALSE`: load the cached file without prompting.
#'   - `NULL` (default): prompt the user interactively.
#' @param ... Additional options passed to [options()].
#'
#' @return Invisibly returns `NULL`. Called for its side effects (setting
#'   R options and printing a configuration summary).
#' @export
#'
#' @examples
#' \dontrun{
#' edstr_config(
#'   edstr_dirname = "output/{Sys.Date()}",
#'   edstr_filename = "my_study",
#'   edstr_text = "note_text",
#'   edstr_overwrite = TRUE
#' )
#' }
#'
edstr_config <- \(
  edstr_dirname,
  edstr_filename,
  edstr_text = NULL,
  edstr_overwrite = NULL,
  ...
) {

  if (!is.character(edstr_dirname) || length(edstr_dirname) != 1L)
    cli_abort("{.arg edstr_dirname} must be a single character string ({.cls character(1)})")
  if (!is.character(edstr_filename) || length(edstr_filename) != 1L)
    cli_abort("{.arg edstr_filename} must be a single character string ({.cls character(1)})")
  if (!is.null(edstr_text) && (!is.character(edstr_text) || length(edstr_text) != 1L))
    cli_abort("{.arg edstr_text} must be a single character string ({.cls character(1)}) or {.code NULL}")
  if (!is.null(edstr_overwrite) && (!is.logical(edstr_overwrite) || length(edstr_overwrite) != 1L))
    cli_abort("{.arg edstr_overwrite} must be {.code TRUE}, {.code FALSE}, or {.code NULL}")

  options(
    edstr_dirname = fs::dir_create(str_glue(edstr_dirname)),
    edstr_filename = str_glue(edstr_filename),
    edstr_text = edstr_text,
    edstr_overwrite = edstr_overwrite,
    ...
  )

  cli_h1("edstr_config"); br()

  cli_alert_info("{.strong Root : {.path {here::here()}}}"); br()

  cli_alert_info(
    "Files will be saved in {.strong {.path {getOption('edstr_dirname')}}} with prefix {.strong {.path {getOption('edstr_filename')}}}"
  )

  br()

  cli_rule()

  invisible(NULL)

}
