#' Generate a configuration file
#'
#' @param dest_dir Path to the destination directory.
#'    A character vector.
#' @param dest_filename Base name of files to create, located in destination directory.
#' @param str Path to a RData file containing string for further text management.
#'    A character vector.
#' @param concepts Path to a RData file containing a list of concepts to use with [edstr_extract()].
#' @param config_name Configuration filename. A character vector. Default is ".config".
#'
#' @return Two hidden objects
#' @export
#'
#' @examples
#'
edstr_config <- \(dest_dir,
                  dest_filename,
                  str,
                  concepts,
                  config_name = ".config") {

dest_dir <- glue::glue(dest_dir)
dest_filename <- glue::glue(dest_filename)

load(str, envir = .GlobalEnv)
load(concepts, envir = .GlobalEnv)

### MANAGE DIRECTORY --------------------------------------------------------------------

if (!file.exists(dest_dir)) {

  dir.create(path = dest_dir)
  dir_status <- "new"

} else dir_status <- "existing"

parent_dir <- stringr::str_remove(getwd(), "[^/]+/?$")
dest_dir <- stringr::str_replace(dest_dir, "^(\\.\\./)", parent_dir)

### ASSIGNEMENT -------------------------------------------------------------------------

assign(config_name,
       dplyr::lst(dir = stringr::str_remove(dest_dir, "/+$"),
                  file = dest_filename,
                  save = glue::glue("{dir}/{file}"),
                  str = .str,
                  concepts = .concepts),
              envir = .GlobalEnv)

assign(".config_name",
       config_name,
       envir = .GlobalEnv)

### CLI ---------------------------------------------------------------------------------

dir_status <- cli::style_underline(dir_status)
dirname <- with(get(config_name), dir)
filename <- cli::col_red(with(get(config_name), file))
config_name <- cli::col_green(config_name)

cli::cli_h1("edstr_config")
cli::cli_text("\n\n")
cli::cli_alert_info("{.strong working directory:} {getwd()}")
cli::cli_text("\n\n")
cli::cli_alert_info("{.strong destination}")
cli::cli_ul()
  cli::cli_li("{dir_status} directory: {.path {dirname}}")
  cli::cli_li("filename: {filename}")
cli::cli_end()
cli::cli_text("\n\n")
cli::cli_alert_info("{.strong configuration file:} {config_name}")
cli::cli_text("\n\n")
cli::cli_rule()

}
