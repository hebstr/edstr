#' config
#'
#' @param config_name config_name
#' @param dest_dir dest_dirname
#' @param dest_filename dest_filename
#' @param text text
#' @param replace replace
#' @param concepts concepts
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_config <- \(config_name = ".config",
                  dest_dir,
                  dest_filename,
                  text = "texte",
                  replace = NULL,
                  concepts = NULL) {

  dest_dir <- glue(dest_dir)
  dest_filename <- glue(dest_filename)
  text <- glue(text)

  if (!is.null(replace)) replace <- get(load(replace))

  if (!is.null(concepts)) concepts <- get(load(concepts))

### DIRECTORY ------------------------------------------------------------------

  if (!file.exists(dest_dir)) {

    dir.create(path = dest_dir, recursive = TRUE)

  }

  dest_dir_norm <- dest_dir |> str_remove("/+$") |> normalize_dir()

### ASSIGN ---------------------------------------------------------------------

  config_list <-
  list(dir = dest_dir_norm,
       file = dest_filename,
       text = text,
       replace = replace,
       concepts = concepts)

  assign(config_name,
         config_list,
         envir = .GlobalEnv)

  assign(".config_name",
         config_name,
         envir = .GlobalEnv)

### CLI ------------------------------------------------------------------------

  .col <- \(x) col_blue(glue(x))

  cli_config_name <- .col(config_name)
  cli_dirname <- .col("{config_name}$dir ==")
  cli_filename <- .col("{config_name}$file == '{dest_filename}'")
  cli_text <- .col("{config_name}$text == '{text}'")
  cli_replace <- .col("{config_name}$replace")
  cli_concepts <- .col("{config_name}$concepts")

  cli_h1("edstr_config")
  cli_text("\n\n")

  cli_alert_info("{.strong R\u00e9pertoire par d\u00e9faut : {.path {here()}}}")
  cli_text("\n\n")

  cli_alert_success("{.strong Objet assign\u00e9 dans l'environnement global : {cli_config_name}}")
  cli_ul()
  cli_ul()
    cli_li("Emplacement : {cli_dirname} {.path {config_list$dir}}")
    cli_li("Nom de fichier : {cli_filename}")
    cli_li("Variable texte : {cli_text}")
    if (!is.null(replace)) cli_li("R\u00e8gles de remplacement (optionnel) : {cli_replace}")
    if (!is.null(concepts)) cli_li("Liste de concepts (optionnel) : {cli_concepts}")
  cli_end()
  cli_text("\n\n")

  cli_rule()

}
