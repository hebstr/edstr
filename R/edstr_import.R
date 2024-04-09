#' Importer des caractéristiques depuis une base de données
#'
#' @param config
#' @param query
#' @param connect_dir
#' @param user
#' @param head
#' @param load
#'
#' @return A tibble
#' @export
#'
#' @examples
#'
edstr_import <- \(query = NULL,
                  connect_dir = NULL,
                  user = NULL,
                  head = NULL,
                  load = FALSE,
                  config = get(.config_name)) {

  cli_error_config()

  if (is.character(config)) config <- get(config)

  config_dir <- with(config, dir)
  config_file <- glue::glue("{with(config, file)}_import")
  config_save <- glue::glue("{config_dir}/{config_file}.RData")

  query <- glue::glue(query)

  cli::cli_h1("edstr_import")
  cli::cli_text("\n\n")

  if (!load) {

### CONNECT -------------------------------------------------------------------------------

    Sys.setenv(JAVA_HOME = glue::glue("{connect_dir}/jdk1.8.0_261"))

    tns <- utils::read.table(glue::glue("{connect_dir}/tns.txt"))

    .con <-
    RJDBC::dbConnect(drv = RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",
                                       classPath = glue::glue("{connect_dir}/ojdbc6.jar")),
                     url = glue::glue("jdbc:oracle:thin:@//{tns}"),
                     user = user,
                     password = getPass::getPass())

    assign(".con", .con, envir = .GlobalEnv)

### QUERY -------------------------------------------------------------------------------

    cli::cli_progress_step("Import from db (user: {.strong {user}})")

    if (!stringr::str_starts(query, "(?i)\\s*SELECT")) {

      query <- readr::read_lines(query)
      query <- query[!stringr::str_starts(query, "--")]

      query_flatten <- \(replace) {

        query |>
          stringr::str_replace_all(c("$" = replace)) |>
          stringr::str_flatten()

      }

      query_flatten("\n") |>
        stringr::str_view() |>
        print()

      query <- query_flatten(" ")

    }

    if (!is.null(head)) {

      query <- glue::glue("{query} FETCH FIRST {head} ROWS ONLY")

    }

    data_import <-
    .con |>
      dplyr::tbl(dplyr::sql(query)) |>
      dplyr::collect() |>
      rlang::set_names(tolower)

### SAVE ---------------------------------------------------------------------------------

    cli::cli_progress_step("Saving {.strong {config_file}}")

    assign(config_file, data_import, envir = .GlobalEnv)

    save(list = config_file, file = config_save)

    cli::cli_progress_done()

### CLI ---------------------------------------------------------------------------------

    cli::cli_alert_success("{.strong {config_file}} saved to {.path {config_save}}")
    cli::cli_text("\n\n")
    cli::cli_alert_info("{.strong Dimensions}")
    cli::cli_ul()
      cli::cli_li("{nrow(data_import)} observations")
      cli::cli_li("{ncol(data_import)} variables")
      cli::cli_end()
    cli::cli_text("\n\n")
    cli::cli_rule()

  } else {

    eval(cli_load())

  }

}
