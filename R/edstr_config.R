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

  cli_h1("edstr_config")
  cli_text("\n\n")

  cli_alert_info("{.strong R\u00e9pertoire parent : {.path {here::here()}}}")
  cli_text("\n\n")

  cli_alert_info("Emplacement d\u00e9fini : {.strong {.path {getOption('edstr_dirname')}}}")
  cli_alert_info("Les fichiers seront nomm\u00e9s avec le pr\u00e9fixe {.strong {.path {getOption('edstr_filename')}}}")
  cli_text("\n\n")

  cli_rule()

}
