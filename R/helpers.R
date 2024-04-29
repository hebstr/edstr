str_u <- \(...) {

  stringr::str_c(c(...), collapse = "|")

}


easy_replace <- \(...,
                  replace = "</>") {

  col_replace <- cli::col_br_red(replace)
  col_replace <- glue::glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
  purrr::map(c(...),
             ~ rlang::list2('{glue::glue("<p>.*({.}).*</p>")}' := replace) |>
               unlist())

  replace_list <-
    rlang::list2("(\n*{replace})+\n*" := col_replace)

  unlist(append(str_list, replace_list))

}


concepts_reduce <- \(x) {

  x |>
    purrr::pluck_depth() |>
    seq() |>
    purrr::reduce(~ purrr::list_flatten(.),
                  .init = x)

}


cli_error_config <- \() {

  if (!exists(".config_name")) {

    cli::cli_abort(c("Configuration file doesn't exists!",
                     "i" = "Please create it first with {.fn edstr_config}"))

  }

}


cli_load <- \() {

  rlang::expr({

    if (!file.exists(config_save)) {

      cli::cli_abort(c("{.strong {config_file}} not found in {.path {config_dir}}",
                       "i" = "Please create {.strong {config_file}} first with {.field load = FALSE}
                       or change directory in {.fn edstr_config}"))

    }

    cli::cli_progress_step("Loading {.strong {config_file}}")

    load(config_save, envir = .GlobalEnv)

    cli::cli_progress_done()

    cli::cli_alert_success("{.strong {config_file}} loaded from {.path {config_save}}")
    cli::cli_text("\n\n")
    cli::cli_rule()

  })

}
