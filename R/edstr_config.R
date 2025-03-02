#' Title
#'
#' @param dest_dir
#' @param dest_filename
#' @param config_dir
#' @param config_name
#' @param text
#' @param str
#' @param concepts
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_config <- \(dest_dir,
                  dest_filename,
                  config_dir = NULL,
                  config_name = ".config",
                  text = "text",
                  str = NULL,
                  concepts = NULL) {

  dest_dir <- glue(dest_dir)
  dest_filename <- glue(dest_filename)
  config_dir <- if (!is.null(config_dir)) glue(config_dir) else NULL
  text <- glue(text)

  if (!is.null(str)) {

    str <- load(file.path(config_dir, str))
    str_data <- get(str)

  } else str_data <- NULL

  if (!is.null(concepts)) {

    concepts <- load(file.path(config_dir, concepts))
    concepts_data <- get(concepts)

  } else concepts_data <- NULL

### DIRECTORY ------------------------------------------------------------------

  if (!file.exists(dest_dir)) {

    dir.create(path = dest_dir, recursive = TRUE)

  }

### ASSIGN ---------------------------------------------------------------------

  assign(config_name,
         list(dir = str_remove(dest_dir, "/+$"),
              file = dest_filename,
              text = text,
              str = str_data,
              concepts = concepts_data),
         envir = .GlobalEnv)

  assign(".config_name",
         config_name,
         envir = .GlobalEnv)

### CLI ------------------------------------------------------------------------

  dirname <- with(get(config_name), dir)
  filename <- col_red(with(get(config_name), file))
  config_name <- col_red(config_name)
  text <- col_red(text)
  str <- col_red(str)
  concepts <- col_red(concepts)

  cli_h1("edstr_config")
  cli_text("\n\n")

  cli_alert_success("{.strong Répertoire de projet:} {.path {here()}}")
  cli_text("\n\n")

  cli_alert_success("{.strong Config}")
  cli_ul()
  cli_ul()
    if (!is.null(config_dir)) cli_li("Dossier source : {.path {normalize_dir(config_dir)}}")
    cli_li("Objet : {config_name}")
  cli_end()
  cli_text("\n\n")

  cli_alert_success("{.strong Assignations}")
  cli_ul()
    cli_li("Dossier de destination : {.path {normalize_dir(dirname)}}")
    cli_li("Nom de fichier : {filename}")
    cli_li("Variable texte : {text}")
    if (!is.null(str_data)) cli_li("Règles de nettoyage : {str}")
    if (!is.null(concepts_data)) cli_li("Liste de concepts : {concepts}")
  cli_end()
  cli_text("\n\n")

  cli_rule()

}
