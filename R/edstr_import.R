.import_save <- \(
  query,
  head,
  lower,
  user,
  password,
  connect_dir,
  tns,
  config,
  ...
) {
  if (is.null(query)) {
    cli_abort(c(
      "{.arg query}: provide a path to a .sql file or a SQL query string"
    ))
  }

  if (is.null(connect_dir)) {
    cli_abort(c(
      "{.arg connect_dir} is not set",
      "i" = "Set {.arg edstr_connect_dir} in {.fn edstr_config} or pass {.arg connect_dir} explicitly"
    ))
  }

  if (!fs::file_exists(connect_dir)) {
    cli_abort(c(
      "{.arg connect_dir} file not found: {.path {connect_dir}}",
      "i" = "Check the path passed to {.arg connect_dir} or set in {.fn edstr_config}"
    ))
  }

  # an empty password reaches the server as a failed attempt against the
  # account's lockout counter rather than as a missing credential, and a prompt
  # answers itself with `""` when nobody is there to type
  if (is.null(password)) {
    if (!rlang::is_interactive()) {
      cli_abort(c(
        "{.arg password} is required outside an interactive session",
        "i" = "Pass {.arg password} explicitly: there is no prompt to answer here"
      ))
    }
  } else if (!nzchar(password)) {
    cli_abort(c(
      "{.arg password} is empty",
      "i" = "Pass a non-empty password, or omit it to be prompted for one"
    ))
  }

  cli_h1("edstr_import")
  br()

  tic("Full steps")

  .gc_r_java()

  dbconfig <- config::get(file = connect_dir)

  password <- password %||%
    if (rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()) {
      rstudioapi::askForPassword()
    } else {
      # `readline()` would echo the credential into the terminal scrollback
      rlang::check_installed(
        "askpass",
        reason = "to read the password without echoing it."
      )
      askpass::askpass("Password: ")
    }

  connection <- DatabaseConnector::connect(
    dbms = dbconfig$db$driver,
    pathToDriver = dbconfig$db$path,
    connectionString = paste0(dbconfig$db$adress, dbconfig$tns[[tns]]),
    user = user,
    password = password,
    ...
  )

  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  on.exit(.gc_r_java(), add = TRUE)

  query <- read_query(query)

  if (!is.null(head)) {
    if (!is.numeric(head) || length(head) != 1 || !is.finite(head) || head < 1) {
      cli_abort(
        "{.arg head} must be a positive integer"
      )
    }

    query <- str_glue("{query} FETCH FIRST {as.integer(head)} ROWS ONLY")
  }

  br()
  cli_progress_step("Import")

  data_import <- tbl(connection, sql(query)) |> collect()

  if (lower) {
    data_import <- set_names(data_import, tolower)
  }

  cli_progress_done()
  br()

  cli_save(
    data = data_import,
    config_file = config$file,
    config_save = config$save
  )

  br()
  toc()
  br()

  data_import
}

.import_load <- \(config) {
  cli_h1("edstr_import")
  br()

  cli_load(
    dir = config$dir,
    file = config$file,
    save = config$save
  )
}

#' Import data from the EDS
#'
#' Execute a SQL query against an Oracle database and save the result as a
#' Parquet file. If a cached file already exists, behaviour depends on the
#' `edstr_overwrite` option (see [edstr_config()]).
#'
#' Requires [edstr_config()] to be called first.
#'
#' @param query `<character(1)>` A SQL query string or a path to a `.sql`
#'   file. If `NULL`, the function errors.
#' @param head `<integer(1)>` Optional. Limit the query to the first `head`
#'   rows (appends `FETCH FIRST ... ROWS ONLY`).
#' @param lower `<logical(1)>` If `TRUE` (default), column names are
#'   converted to lowercase.
#' @param user `<character(1)>` Database username.
#' @param password `<character(1)>` Database password. Defaults to an
#'   interactive prompt via `rstudioapi::askForPassword()` (if available)
#'   or `askpass::askpass()`, neither of which echoes. Required outside an
#'   interactive session, where a prompt would answer itself with `""` and
#'   spend an authentication attempt. An empty string is rejected everywhere,
#'   `Sys.getenv()` returning one for an unset variable.
#' @param connect_dir `<character(1)>` Path to the YAML connection
#'   configuration file read by [config::get()]. Defaults to
#'   `getOption("edstr_connect_dir")`, set by [edstr_config()].
#' @param tns `<character(1)>` TNS alias to select from the connection
#'   configuration (default `"vlp"`).
#' @param ... Additional arguments passed to
#'   [DatabaseConnector::connect()].
#'
#' @return A [data.frame] with the imported data.
#' @export
#'
#' @examples
#' \dontrun{
#' edstr_config(edstr_dirname = "output", edstr_filename = "my_study")
#'
#' df <- edstr_import(
#'   query = "sql/my_query.sql",
#'   head = 100,
#'   user = "my_user"
#' )
#' }
#'
edstr_import <- \(
  query = NULL,
  head = NULL,
  lower = TRUE,
  user = NULL,
  password = NULL,
  connect_dir = getOption("edstr_connect_dir"),
  tns = "vlp",
  ...
) {
  config <- check_config("import")

  fun_save <- \() {
    .import_save(
      query,
      head,
      lower,
      user,
      password,
      connect_dir,
      tns,
      config,
      ...
    )
  }

  if (fs::file_exists(config$save)) {
    cli_check(
      config_file = config$file,
      fun_save = fun_save,
      fun_load = \() .import_load(config)
    )
  } else {
    fun_save()
  }
}
