#' extract
#'
#' @param query query
#' @param head head
#' @param lower lower
#' @param user user
#' @param password password
#' @param connect_dir connect_dir
#' @param tns tns
#' @param load load
#' @param ... ...
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_import <- \(
  query = NULL,
  head = NULL,
  lower = TRUE,
  user = NULL,
  password = askForPassword(),
  connect_dir = "/opt/oracle/instantclient_23_7/connect/dbconnect.yml",
  tns = "vlp",
  load = FALSE,
  ...
) {

  config <- check_config("import")

  cli_h1("edstr_import"); br()

  if (!load) {

    tic("Full steps")

### CONNECT --------------------------------------------------------------------

    .gc_r_java()

    dbconfig <- config::get(file = connect_dir)

    connection <- DatabaseConnector::connect(
      dbms = dbconfig$db$driver,
      pathToDriver = dbconfig$db$path,
      connectionString = paste0(dbconfig$db$adress, dbconfig$tns[[tns]]),
      user = user,
      password = password,
      ...
    )

    # connection <- DBI::dbConnect(
    #   drv = duckdb::duckdb(),
    #   dbdir = "_extra/collect/data/test.duckdb"
    # )

    br()

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

      query <- str_glue("{query} FETCH FIRST {head} ROWS ONLY")

    }

    get_query <- \() tbl(connection, sql(query))

### IMPORT ---------------------------------------------------------------------

    br(); if (!is.null(user)) cli_progress_step("Import (user: {.strong {user}})")

    data_import <- collect(get_query())

    if (lower) data_import <- set_names(data_import, tolower)

    cli_progress_done()

    cli_save(
      data = data_import,
      config_file = config$file,
      config_save = config$save
    )

    br(); toc(); br()

    DatabaseConnector::disconnect(connection)
    # DBI::dbDisconnect(connection)

    .gc_r_java()

    return(data_import)

### LOAD -----------------------------------------------------------------------

  } else {

    cli_load(
      dir = config$dir,
      file = config$file,
      save = config$save
    )

  }

}
