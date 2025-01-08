str_u <- \(...) {

  str_c(c(...), collapse = "|")

}


normalize_dir <- \(dir) {

  dir |>
    glue() |>
    normalizePath() |>
    str_replace_all("\\\\", "/")

}


easy_replace <- \(...,
                  replace = "</>") {

  col_replace <- col_br_red(replace)
  col_replace <- glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
  c(...) |>
    map(~ list2('{glue("<p>.*({.}).*</p>")}' := replace) |>
          unlist())

  replace_list <- list2("(\n*{replace})+\n*" := col_replace)

  unlist(append(str_list, replace_list))

}


cli_error_config <- \(dest_dir = NULL,
                      dest_filename = NULL) {

  if (!is.null(dest_dir) && !is.null(dest_filename)) {

    if (!file.exists(dest_dir)) dir.create(path = dest_dir, recursive = TRUE)

    list(dir = str_remove(dest_dir, "/+$"),
         file = dest_filename)

  } else {

    cli_abort(c("Config file doesn't exists!",
                "i" = "Create it first with {.fn edstr_config}"))

  }

}


cli_save <- \(data,
              config_file,
              config_save,
              prop_import = NULL) {

  cli_progress_step("Saving {.strong {config_file}}")

  assign(config_file, data, envir = .GlobalEnv)

  save(list = config_file, file = config_save)

  cli_progress_done()

  cli_alert_success("{.strong {config_file}} saved in {.path {here(config_save)}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Dimensions}")
  cli_ul()
    cli_li("{nrow(data)} documents {prop_import}")
    cli_li("{ncol(data)} features")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}


cli_load <- \(config_dir,
              config_file,
              config_save) {

  if (!file.exists(config_save)) {

    cli_abort(c("{.strong {config_file}} not found in {.path {here(config_dir)}}",
                "i" =
                  "Create {.strong {config_file}} first with {.field load = FALSE}
                  or change directory in {.fn edstr_config} with {.field dest_dir}"))

  }

  cli_progress_step("Loading {.strong {config_file}}")

  load(config_save, envir = .GlobalEnv)

  cli_progress_done()

  cli_alert_success("{.strong {config_file}} loaded from {.path {here(config_save)}}")
  cli_text("\n\n")
  cli_rule()

}
