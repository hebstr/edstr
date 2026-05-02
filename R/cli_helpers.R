cli_save <- \(data, config_file, config_save) {
  cli_progress_step("Saving file {.strong {config_file}}")

  saveRDS(data, file = config_save)

  cli_progress_done()

  cli_alert_success(
    "File {.strong {config_file}} saved to {.strong {.path {config_save}}}"
  )
  br()

  cli_alert_info("{.strong Dimensions}")
  cli_ul()
  cli_ul()
  cli_li("{nrow(data)} documents")
  cli_li("{ncol(data)} variables")
  cli_end()

  br()
  cli_rule()

  data
}

cli_load <- \(dir, file, save) {
  cli_progress_step("Loading file {.strong {file}}")

  .load <- readRDS(save)

  cli_progress_done()

  cli_alert_success(
    "File {.strong {file}} loaded from {.strong {.path {save}}}"
  )

  br()
  cli_rule()

  .load
}

cli_check <- \(config_file, fun_save, fun_load) {
  overwrite <- getOption("edstr_overwrite")

  if (isTRUE(overwrite)) {
    return(fun_save())
  }
  if (isFALSE(overwrite)) {
    return(fun_load())
  }

  br()
  cli_alert_warning("File {.strong {config_file}} already exists.")

  if (!rlang::is_interactive()) {
    cli_abort(
      "File {.strong {config_file}} already exists. Set option {.code edstr_overwrite} to {.val TRUE} or {.val FALSE} in non-interactive sessions."
    )
  }

  choice <- menu(
    choices = c(
      "Load existing file",
      "Overwrite existing file",
      "Cancel"
    ),
    title = NULL
  )

  if (choice == 1) {
    fun_load()
  } else if (choice == 2) {
    fun_save()
  } else {
    cli_abort("Operation cancelled.")
  }
}
