#' config
#'
#' @param edstr_dirname edstr_dirname
#' @param edstr_filename edstr_filename
#' @param ... ...
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_config <- \(
  edstr_dirname,
  edstr_filename,
  ...
) {

  options(
    edstr_dirname = fs::dir_create(str_glue(edstr_dirname)),
    edstr_filename = str_glue(edstr_filename),
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
