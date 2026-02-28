#' config
#'
#' @param edstr_dirname edstr_dirname
#' @param edstr_filename edstr_filename
#' @param edstr_text edstr_text
#' @param edstr_overwrite edstr_overwrite
#' @param ... ...
#'
#' @return value
#' @export
#'
#' @examples "example"
#'
edstr_config <- \(
  edstr_dirname,
  edstr_filename,
  edstr_text = NULL,
  edstr_overwrite = NULL,
  ...
) {

  options(
    edstr_dirname = fs::dir_create(str_glue(edstr_dirname)),
    edstr_filename = str_glue(edstr_filename),
    edstr_text = edstr_text,
    edstr_overwrite = edstr_overwrite,
    ...
  )

  set_name <- \(x) paste0("{.strong {.path {getOption('edstr_", x, "')}}}")

  cli_h1("edstr_config"); br()

  cli_alert_info("{.strong R\u00e9pertoire parent : {.path {here::here()}}}"); br()

  cli_alert_info(str_glue(
    "Les fichiers seront enregistrés dans le r\u00e9pertoire {set_name('dirname')} et nomm\u00e9s avec le pr\u00e9fixe {set_name('filename')}"
  ))

  br()

  cli_rule()

}
