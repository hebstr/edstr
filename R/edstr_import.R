#' Title
#'
#' @param dest_dir
#' @param dest_filename
#' @param connect_dir
#' @param tns
#' @param user
#' @param password
#' @param control
#' @param query
#' @param head
#' @param to_lower
#' @param load
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_import <- \(dest_dir = NULL,
                  dest_filename = NULL,
                  connect_dir = "../R/dbconnect",
                  tns = "tns",
                  user = "w_etudes",
                  password = getPass::getPass(),
                  control = FALSE,
                  query = NULL,
                  head = NULL,
                  to_lower = TRUE,
                  load = FALSE) {

  if (!exists(".config_name")) {

    config <- cli_error_config(dest_dir, dest_filename)

  } else config <- get(.config_name)

  config_dir <- with(config, dir)
  config_file <- glue("{with(config, file)}_import")
  config_save <- glue("{config_dir}/{config_file}.RData")

  connect_dir <- glue(connect_dir)
  query <- glue(query)

  cli_h1("edstr_import")
  cli_text("\n\n")

  if (!load) {

    tic("Full steps")

### CONNECT --------------------------------------------------------------------

    RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",
                classPath = "/opt/oracle/instantclient_23_7/ojdbc17.jar")

    tns <- read_lines(glue("{connect_dir}/{tns}.txt"))
    adress <- glue("jdbc:oracle:thin:@{tns}")

    conn <-
    DatabaseConnector::connect(dbms = "oracle",
                               pathToDriver = connect_dir,
                               connectionString = adress,
                               user = user,
                               password = password)

    cli_text("\n\n")

### QUERY ----------------------------------------------------------------------

    if (!str_starts(query, "(?i)\\s*SELECT")) {

      query <- read_lines(file = query, skip_empty_rows = TRUE)

      query <- query[!str_starts(query, "\\s*--")]

      query_flatten <- \(replace) {

        query |>
          str_replace("$", replace) |>
          str_flatten()

      }

      query_flatten("\n") |>
        str_remove("\n$") |>
        str_view() |>
        print()

      query <- query_flatten(" ")

    }

    if (!is.null(head)) {

      query <- glue("{query} FETCH FIRST {head} ROWS ONLY")

    }

### IMPORT ---------------------------------------------------------------------

    if (!control) {

      cli_text("\n\n")
      cli_progress_step("Import (user: {.strong {user}})")

      data_import <- tbl(conn, sql(query)) |> collect()

      if (to_lower) {

        data_import <- data_import |> set_names(tolower)

      }

      cli_progress_done()

### CLI ------------------------------------------------------------------------

      cli_save(data_import,
               config_file,
               config_save)

      cli_text("\n\n")
      toc()
      cli_text("\n\n")

    } else {

      cli_text("\n\n")
      cli_progress_step("Control (user: {.strong {user}})")

      data_import <- tbl(conn, sql(query)) |> collect()

      cli_progress_done()
      cli_text("\n\n")
      toc()
      cli_text("\n\n")

      print(data_import)

      cli_text("\n\n")
      cli_rule()

    }

    DatabaseConnector::disconnect(conn)

  } else {

    cli_load(dir = config_dir,
             file = config_file,
             save = config_save)

  }

  invisible(gc())

}
