#' Définir une configuration initale
#'
#' @param dest_dir Chemin vers le répertoire de destination. Un vecteur de caractères.
#' @param dest_filename Base name of files to create, located in destination
#'   directory.
#' @param str Path to a RData file containing string for further text
#'   management. Un vecteur de caractères.
#' @param split
#' @param concepts Path to a RData file containing a list of concepts to use
#'   with [edstr_extract()].
#' @param text
#' @param config_name Nom du fichier de configuration. Un vecteur de caractères. Nommé
#'   ".config" par défaut.
#'
#' @return Two hidden objects
#' @export
#'
#' @examples
#'
edstr_config <- \(dest_dir,
                  dest_filename,
                  str = NULL,
                  split = NULL,
                  concepts = NULL,
                  text = NULL,
                  config_name = ".config") {

  dest_dir <- glue(dest_dir)
  dest_filename <- glue(dest_filename)

  if (!is.null(str)) {

    str <- glue(str) |> load()
    str_data <- get(str)

  } else str_data <- NULL

  if (!is.null(concepts)) {

    concepts <- glue(concepts) |> load()
    concepts_data <- get(concepts)

  } else concepts_data <- NULL

  if (!is.null(text)) {

    text <- glue(text)

  } else text <- NULL

### MANAGE DIRECTORY -----------------------------------------------------------

  if (!file.exists(dest_dir)) {

    dir.create(path = dest_dir, recursive = TRUE)
    dir_status <- "New"

  } else dir_status <- "Existing"

  parent_dir <- str_remove(getwd(), "[^/]+/?$")
  dest_dir <- str_replace(dest_dir, "^(\\.\\./)", parent_dir)

### ASSIGNMENT -----------------------------------------------------------------

  if (!is.null(split)) {

    split <- glue(split) |> load()

    str_data <-
    list_modify(get(str),
                extract_split =
                  list(sect = with(get(split), str)))

  }

  assign(config_name,
         lst(dir = str_remove(dest_dir, "/+$"),
             file = dest_filename,
             str = str_data,
             concepts = concepts_data,
             text = text),
         envir = .GlobalEnv)

  assign(".config_name",
         config_name,
         envir = .GlobalEnv)

### CLI ------------------------------------------------------------------------

  dirname <- with(get(config_name), dir)
  filename <- col_red(with(get(config_name), file))
  config_name <- col_green(config_name)
  str <- col_green(str)
  concepts <- col_green(concepts)
  split <- col_green(split)
  text <- col_green(text)

  cli_h1("edstr_config")
  cli_text("\n\n")
  cli_alert_info("{.strong Working directory:} {.path {getwd()}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Destination}")
  cli_ul()
    cli_li("{dir_status} directory: {.path {dirname}}")
    cli_li("Filename: {filename}")
  cli_end()
  cli_text("\n\n")
  cli_alert_info("{.strong Configuration file:} {.strong {config_name}}")
  cli_ul()
    cli_li("Replacement list: {str}")
    cli_li("Concepts list: {concepts}")
    cli_li("Split list: {split}")
    cli_li("Text input: {text}")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}
