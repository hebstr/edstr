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

    cli_abort(c("Le fichier de configuration n'existe pas",
                "i" =
                  "Créer ce fichier dans un premier temps avec {.fn edstr_config},
                  ou bien indiquer un répertoire par défaut avec {.field dest_dir}
                  et un nom de fichier par défaut avec {.field dest_filename}"))

  }

}


cli_error_data <- \(data, fun) {

  cli_abort(c("Le fichier {.strong {data}} n'est pas chargé",
            "i" =
              "Créer/charger le fichier {.strong {data}} dans un premier
              temps avec {.fn edstr_{glue(fun)}} ou bien utiliser un autre fichier"))

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


cli_load <- \(dir, file, save) {

  if (!file.exists(dir)) {

    cli_abort(c("Le fichier {.strong {file}} ne peut être chargé car le dossier
                {.path {here(dir)}} n'existe pas",
                "i" =
                  "Créer le fichier {.strong {file}} dans un premier temps avec
                  {.field load = FALSE} ou bien charger un autre fichier"))

  }

  if (!file.exists(save)) {

    cli_abort(c(" Le fichier {.strong {file}} n'est pas retrouvé dans le dossier
                {.path {here(dir)}}",
                "i" =
                  "Créer le fichier {.strong {file}} dans un premier temps avec
                  {.field load = FALSE} ou bien changer le répertoire par défaut
                  dans {.fn edstr_config} avec {.field dest_dir}"))

  }

  cli_progress_step("Chargement du fichier {.strong {file}}")

  load(save, envir = .GlobalEnv)

  cli_progress_done()

  cli_alert_success("Ficher {.strong {file}} chargé depuis {.path {here(save)}}")
  cli_text("\n\n")
  cli_rule()

}
