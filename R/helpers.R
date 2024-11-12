str_u <- \(...) {

  str_c(c(...), collapse = "|")

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


concepts_reduce <- \(x) {

  x |>
    pluck_depth() |>
    seq() |>
    reduce(~ list_flatten(.),
           .init = x)

}


cli_error_config <- \(dest_dir = NULL,
                      dest_filename = NULL) {

  if (!is.null(dest_dir) && !is.null(dest_filename)) {

    if (!file.exists(dest_dir)) dir.create(path = dest_dir, recursive = TRUE)

    list(dir = str_remove(dest_dir, "/+$"),
         file = dest_filename)

  } else {

    cli_abort(c("Configuration file doesn't exists!",
                "i" = "Please create it first with {.fn edstr_config}"))

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

  cli_alert_success("{.strong {config_file}} saved to {.path {config_save}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Dimensions}")
  cli_ul()
    cli_li("{nrow(data)} observations {prop_import}")
    cli_li("{ncol(data)} variables")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}


cli_load <- \(config_dir,
              config_file,
              config_save) {

  if (!file.exists(config_save)) {

    cli_abort(c("{.strong {config_file}} not found in {.path {config_dir}}",
                "i" =
                  "Please create {.strong {config_file}} first with {.field load = FALSE}
                  or change directory in {.fn edstr_config}"))

  }

  cli_progress_step("Loading {.strong {config_file}}")

  load(config_save, envir = .GlobalEnv)

  cli_progress_done()

  cli_alert_success("{.strong {config_file}} loaded from {.path {config_save}}")
  cli_text("\n\n")
  cli_rule()

}


check_fonts <- \(...,
                 .auto = NULL,
                 .abort = FALSE) {

  if (!is.null(.auto)) {

    check_dots_empty()

    if (!check_fonts(.auto)) "trebuchet ms" else .auto

  } else {

    fonts <- unlist(...)

    system <- unique(systemfonts::system_fonts()$family)
    system <- glue("\\b{system}\\b")
    system <- glue("(?i){str_u(system)}")

    is_installed <- str_detect(fonts, system)

    if (FALSE %in% is_installed) {

      which_font <-
      data.frame(fonts, is_installed) |>
        filter(!is_installed) |>
        pull(fonts)

      if (.abort) cli_abort("{which_font} font{?s} {?is/are} not installed")

      return(FALSE)

    }

    return(TRUE)

  }

}

