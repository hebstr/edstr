#' Définir une configuration initale
#'
#' @param dest_dir Chemin vers le répertoire de destination. Un vecteur de caractères.
#' @param dest_filename Base name of files to create, located in destination
#'   directory.
#' @param str Path to a RData file containing string for further text
#'   management. Un vecteur de caractères.
#' @param concepts Path to a RData file containing a list of concepts to use
#'   with [edstr_extract()].
#' @param text
#' @param config_name Nom du fichier de configuration. Un vecteur de caractères. Nommé
#'   ".config" par défaut.
#' @param split
#'
#' @return Two hidden objects
#' @export
#'
#' @examples
#'
edstr_config <- \(dest_dir,
                  dest_filename,
                  str = NULL,
                  concepts = NULL,
                  split = NULL,
                  text = NULL,
                  config_name = ".config") {

  dest_dir <- glue::glue(dest_dir)
  dest_filename <- glue::glue(dest_filename)

  if (!is.null(str)) {

    str <- glue::glue(str) |> load()
    str_data <- get(str)

  } else str_data <- NULL

  if (!is.null(concepts)) {

    concepts <- glue::glue(concepts) |> load()
    concepts_data <- get(concepts)

  } else concepts_data <- NULL

  if (!is.null(text)) {

    text <- glue::glue(text)

  } else text <- NULL

  ### MANAGE DIRECTORY --------------------------------------------------------------------

  if (!file.exists(dest_dir)) {

    dir.create(path = dest_dir, recursive = TRUE)
    dir_status <- "New"

  } else dir_status <- "Existing"

  parent_dir <- stringr::str_remove(getwd(), "[^/]+/?$")
  dest_dir <- stringr::str_replace(dest_dir, "^(\\.\\./)", parent_dir)

  ### ASSIGNMENT -------------------------------------------------------------------------

  if (!is.null(split)) {

    split <- glue::glue(split) |> load()

    str_data <-
    purrr::list_modify(get(str),
                       extract_split =
                         list(sect = str_u("(?<=:)\\s*(?=(<br/>\\s*)*</p>)",
                                           with(get(split), str))))

  }

  assign(config_name,
         dplyr::lst(dir = stringr::str_remove(dest_dir, "/+$"),
                    file = dest_filename,
                    str = str_data,
                    concepts = concepts_data,
                    text = text),
         envir = .GlobalEnv)

  assign(".config_name",
         config_name,
         envir = .GlobalEnv)

  ### CLI ---------------------------------------------------------------------------------

  dirname <- with(get(config_name), dir)
  filename <- cli::col_red(with(get(config_name), file))
  config_name <- cli::col_green(config_name)
  str <- cli::col_green(str)
  concepts <- cli::col_green(concepts)
  split <- cli::col_green(split)
  text <- cli::col_green(text)

  cli::cli_h1("edstr_config")
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Working directory:} {.path {getwd()}}")
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Destination}")
  cli::cli_ul()
    cli::cli_li("{dir_status} directory: {.path {dirname}}")
    cli::cli_li("Filename: {filename}")
  cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Configuration file:} {.strong {config_name}}")
  cli::cli_ul()
    cli::cli_li("Replacement list: {str}")
    cli::cli_li("Concepts list: {concepts}")
    cli::cli_li("Split list: {split}")
    cli::cli_li("Text input: {text}")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_rule()

}
