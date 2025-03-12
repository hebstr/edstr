#' Title
#'
#' @param config_name
#' @param dest_dir
#' @param dest_filename
#' @param text
#' @param replace
#' @param concepts
#'
#' @return
#' @export
#'
#' @examples
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

### ASSIGN ---------------------------------------------------------------------

  config_list <-
  list(dir = dest_dir |> str_remove("/+$") |> normalize_dir(),
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

  cli <-

  cli_h1("edstr_config")
  cli_text("\n\n")

  cli_alert_info("{.strong Répertoire par défaut :} {.path {here()}}")
  cli_text("\n\n")

  cli_alert_success("{.strong Objet assigné dans l'environnement global : {cli_config_name}}")
  cli_ul()
  cli_ul()
    cli_li("Emplacement : {cli_dirname} {.path {config_list$dir}}")
    cli_li("Nom de fichier : {cli_filename}")
    cli_li("Variable texte : {cli_text}")
    if (!is.null(replace)) cli_li("Règles de remplacement (optionnel) : {cli_replace}")
    if (!is.null(concepts)) cli_li("Liste de concepts (optionnel) : {cli_concepts}")
  cli_end()
  cli_text("\n\n")

  cli_rule()

}
