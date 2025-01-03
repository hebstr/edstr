#' Title
#'
#' @param query
#' @param head
#' @param to_lower
#' @param dest_dir
#' @param dest_filename
#' @param connect_dir
#' @param tns
#' @param user
#' @param password
#' @param load
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_import <- \(query = NULL,
                  head = NULL,
                  to_lower = TRUE,
                  dest_dir = NULL,
                  dest_filename = NULL,
                  connect_dir = with(config, connect),
                  tns = "tns",
                  user = NULL,
                  password = getPass::getPass(),
                  load = FALSE) {

  if (!exists(".config_name")) {

    config <- cli_error_config(dest_dir, dest_filename)

  } else config <- get(.config_name)

  config_dir <- with(config, dir)
  config_file <- glue("{with(config, file)}_import")
  config_save <- glue("{config_dir}/{config_file}.RData")

  query <- glue(query)
  connect_dir <- glue(connect_dir)

  cli_h1("edstr_import")
  cli_text("\n\n")

  if (!load) {

### CONNECT --------------------------------------------------------------------

    Sys.setenv(JAVA_HOME = glue("{connect_dir}/jdk1.8.0_261"))

    tns <- read.table(glue("{connect_dir}/{tns}.txt"))

    .con <-
    RJDBC::dbConnect(drv =
                       RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver",
                                   classPath = glue("{connect_dir}/ojdbc6.jar")),
                     url = glue("jdbc:oracle:thin:@//{tns}"),
                     user = user,
                     password = password)

    assign(".con", .con, envir = .GlobalEnv)

### QUERY ----------------------------------------------------------------------

    if (!str_starts(query, "(?i)\\s*SELECT")) {

      query <-
      read_lines(file = query,
                 skip_empty_rows = TRUE)

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

    cli_text("\n\n")
    cli_progress_step("Import (user: {.strong {user}})")

    data_import <- tbl(.con, sql(query)) |> collect()

    if (to_lower) {

      data_import <- data_import |> set_names(tolower)

    }

    cli_progress_done()

### CLI ------------------------------------------------------------------------

    cli_save(data_import,
             config_file,
             config_save)

  } else {

    cli_load(config_dir,
             config_file,
             config_save)

  }

}
