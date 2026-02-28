.import_save <- \(
  query, head, lower, user, password,
  connect_dir, tns, config,
  ...
) {

  if (is.null(query)) cli_abort(c(
    "{.arg query} : spécifier un chemin vers un fichier .sql
    ou une requête SQL directe"
  ))

  cli_h1("edstr_import"); br()

  tic("Full steps")

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

  # on.exit(DBI::dbDisconnect(connection))

  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  on.exit(.gc_r_java(), add = TRUE)

  query <- read_query(query)

  if (!is.null(head)) {

    if (!is.numeric(head) || length(head) != 1) cli_abort(
      "{.arg head} doit être un entier positif"
    )

    query <- str_glue("{query} FETCH FIRST {as.integer(head)} ROWS ONLY")

  }

  br(); cli_progress_step("Import")

  data_import <- tbl(connection, sql(query)) |> collect()

  if (lower) data_import <- set_names(data_import, tolower)

  cli_progress_done(); br()

  cli_save(
    data = data_import,
    config_file = config$file,
    config_save = config$save
  )

  br(); toc(); br()

  return(data_import)

}

.import_load <- \(config) {

  cli_h1("edstr_import"); br()

  cli_load(
    dir = config$dir,
    file = config$file,
    save = config$save
  )

}

#' import
#'
#' @param query query
#' @param head head
#' @param lower lower
#' @param user user
#' @param password password
#' @param connect_dir connect_dir
#' @param tns tns
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
  ...
) {

  config <- check_config("import")

  fun_save <- \() .import_save(
    query, head, lower, user, password,
    connect_dir, tns, config,
    ...
  )

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
