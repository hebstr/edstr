#' extract
#'
#' @param query query
#' @param head head
#' @param to_lower to_lower
#' @param user user
#' @param password password
#' @param connect_dir connect_dir
#' @param tns tns
#' @param disconnect disconnect
#' @param dest_dir dest_dirname
#' @param dest_filename dest_filename
#' @param collect collect
#' @param load load
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_import <- \(query = NULL,
                  head = NULL,
                  to_lower = TRUE,
                  user = "w_etudes",
                  password = getPass::getPass(),
                  connect_dir = "../R/connect",
                  tns = "tns",
                  disconnect = TRUE,
                  dest_dir = NULL,
                  dest_filename = NULL,
                  collect = TRUE,
                  load = FALSE) {

  if (collect) {

    if (!exists(".config_name")) {

      config <- cli_error_config(dest_dir, dest_filename)

    } else config <- get(.config_name)

    config_dir <- config$dir
    config_file <- glue("{with(config, file)}_import")
    config_save <- glue("{config_dir}/{config_file}.RData")

  }

  connect_dir <- glue(connect_dir)
  query <- glue(query)

  cli_h1("edstr_import")
  cli_text("\n\n")

  if (!load) {

    tic("Full steps")

### CONNECT --------------------------------------------------------------------

    RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",
                classPath = glue("{connect_dir}/ojdbc17.jar")) # "/opt/oracle/instantclient_23_7/ojdbc17.jar"

    tns <- read_lines(glue("{connect_dir}/{tns}.txt"))
    adress <- glue("jdbc:oracle:thin:@{tns}")

    connection <-
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

    get_query <- \(x = connection, y = query) tbl(x, sql(y))

### IMPORT ---------------------------------------------------------------------

    if (collect) {

      cli_text("\n\n")
      cli_progress_step("Import (user: {.strong {user}})")

      data_import <- get_query() |> collect()

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

    } else if (!is.null(head)) {

      return(get_query() |> collect())

    } else {

      return(get_query())

    }

    if (disconnect) {

      DatabaseConnector::disconnect(connection)

    } else {

      assign("connection",
             connection,
             envir = rlang::caller_env())

    }

  } else {

    cli_load(dir = config_dir,
             file = config_file,
             save = config_save)

  }

  invisible(gc())

}
