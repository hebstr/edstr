#' Importer des caractéristiques depuis une base de données
#'
#' @param query
#' @param head
#' @param to_lower
#' @param connect_dir
#' @param tns
#' @param user
#' @param password
#' @param dest_dir
#' @param dest_filename
#' @param load
#'
#' @return A tibble
#' @export
#'
#' @examples
#'
edstr_import <- \(query = NULL,
                  head = NULL,
                  to_lower = TRUE,
                  connect_dir = NULL,
                  tns = "tns",
                  user = NULL,
                  password = getPass::getPass(),
                  dest_dir = NULL,
                  dest_filename = NULL,
                  load = FALSE) {

  if (!exists(".config_name")) {

  config <- cli_error_config(dest_dir, dest_filename)

  } else config <- get(.config_name)

  config_dir <- with(config, dir)
  config_file <- glue::glue("{with(config, file)}_import")
  config_save <- glue::glue("{config_dir}/{config_file}.RData")

  query <- glue::glue(query)
  connect_dir <- glue::glue(connect_dir)

  cli::cli_h1("edstr_import")
  cli::cli_text("\n\n")

  if (!load) {

### CONNECT -------------------------------------------------------------------------------

    Sys.setenv(JAVA_HOME = glue::glue("{connect_dir}/jdk1.8.0_261"))

    tns <- utils::read.table(glue::glue("{connect_dir}/{tns}.txt"))

    .con <-
    RJDBC::dbConnect(drv = RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",
                                       classPath = glue::glue("{connect_dir}/ojdbc6.jar")),
                     url = glue::glue("jdbc:oracle:thin:@//{tns}"),
                     user = user,
                     password = password)

    assign(".con", .con, envir = .GlobalEnv)

### QUERY -------------------------------------------------------------------------------

    if (!stringr::str_starts(query, "(?i)\\s*SELECT")) {

      query <- readr::read_lines(query)
      query <- query[!stringr::str_starts(query, "--")]

      query_flatten <- \(replace) {

        query |>
          stringr::str_replace_all(c("$" = replace)) |>
          stringr::str_flatten()

      }

      query_flatten("\n") |>
        stringr::str_remove("\n$") |>
        stringr::str_view() |>
        print()

      query <- query_flatten(" ")

    }

    if (!is.null(head)) {

      query <- glue::glue("{query} FETCH FIRST {head} ROWS ONLY")

    }

    cli::cli_text("\n\n")
    cli::cli_progress_step("Import with user {.strong {user}}")

    data_import <-
    .con |>
      dplyr::tbl(dplyr::sql(query)) |>
      dplyr::collect()

    if (to_lower) {

      data_import <- data_import |> rlang::set_names(tolower)

    }

    cli::cli_progress_done()

### CLI ---------------------------------------------------------------------------------

    cli_save(data_import,
             config_file,
             config_save)

  } else {

    cli_load(config_dir,
             config_file,
             config_save)

  }

}
