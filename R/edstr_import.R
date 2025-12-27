#' extract
#'
#' @param query query
#' @param head head
#' @param lower lower
#' @param user user
#' @param password password
#' @param connect_dir connect_dir
#' @param tns tns
#' @param collect collect
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
  password = rstudioapi::askForPassword(),
  connect_dir = "/opt/oracle/instantclient_23_7/connect/dbconnect.yml",
  tns = "vlp",
  collect = TRUE,
  load = FALSE,
  ...
) {

  if (collect) config <- check_config("import")

  query <- glue(query)
  connect_dir <- glue(connect_dir)

  cli_h1("edstr_import")
  cli_text("\n\n")

  if (!load) {

    tic("Full steps")

### CONNECT --------------------------------------------------------------------

    withr::local_options(java.parameters = "-Xmx8g")

    gc() ; rJava::J("java.lang.Runtime")$getRuntime()$gc()

    dbconfig <- config::get(file = connect_dir)

    connection <- DBI::dbConnect(
      dbms = dbconfig$db$driver,
      pathToDriver = dbconfig$db$path,
      connectionString = paste0(dbconfig$db$adress, dbconfig$tns[[tns]]),
      user = user,
      password = password,
      ...
    )

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

      data_import <- collect(get_query())

      if (lower) data_import <- set_names(data_import, tolower)

      cli_progress_done()

### CLI ------------------------------------------------------------------------

      cli_save(
        data = data_import,
        config_file = config$file,
        config_save = config$save
      )

      cli_text("\n\n")
      toc()
      cli_text("\n\n")

    } else if (!is.null(head)) {

      return(collect(get_query()))

    } else {

      return(get_query())

    }

    DBI::dbDisconnect(connection)

    invisible(gc()) ; rJava::J("java.lang.Runtime")$getRuntime()$gc()

  } else {

    cli_load(
      dir = config$dir,
      file = config$file,
      save = config$save
    )

  }

}
