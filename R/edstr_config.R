#' Title
#'
#' @param dest_dir
#' @param dest_filename
#' @param connect_dir
#' @param config_dir
#' @param config_name
#' @param text
#' @param str
#' @param split
#' @param concepts
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_config <- \(dest_dir,
                  dest_filename,
                  connect_dir = "../_R_/database_connection",
                  config_dir = "../_R_/config",
                  config_name = ".config",
                  text = "text",
                  str = NULL,
                  split = NULL,
                  concepts = NULL) {

  dest_dir <- glue(dest_dir)
  dest_filename <- glue(dest_filename)

  if (!is.null(str)) {

    str <- load(glue("{config_dir}/{str}"))
    str_data <- get(str)

  } else str_data <- NULL

  if (!is.null(concepts)) {

    concepts <- load(glue("{config_dir}/{concepts}"))
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

    split <- load(glue("{config_dir}/{split}"))

    str_data <- list_modify(get(str), split = list(sect = with(get(split), str)))

  }

  assign(config_name,
         list(dir = str_remove(dest_dir, "/+$"),
              file = dest_filename,
              connect = connect_dir,
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
  split <- col_red(split)

  cli_h1("edstr_config")
  cli_text("\n\n")
  cli_alert_info("{.strong Working directory:} {.path {getwd()}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Destination}")
  cli_ul()
  cli_ul()
    cli_li("{dir_status} directory: {.path {dirname}}")
    cli_li("Filename: {filename}")
  cli_end()
  cli_text("\n\n")
  cli_alert_info("{.strong Connection directory:} {.path {connect_dir}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Config directory:} {.path {config_dir}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Config file:} {config_name}")
  cli_ul()
    cli_li("text: {text}")
    cli_li("str: {str}")
    cli_li("concepts: {concepts}")
    cli_li("split: {split}")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}
