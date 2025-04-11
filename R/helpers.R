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


easy_ano <- \(x, hash_vars, hide_vars, trunc = 25) {

  .ano_fun <- \(x, hash_vars) {

    trunc <- as.character(trunc)

    x |>
      mutate("{hash_vars}" :=
               get(hash_vars) |>
               rlang::hash() |>
               str_remove_all(".{trunc}$"),
             .by = all_of(hash_vars))

  }

  .ano_data <-
  names(x) |>
    str_subset(hash_vars) |>
    reduce(.ano_fun, .init = x) |>
    mutate(across(matches(hide_vars), ~ "---"))

  return(.ano_data)

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
                  "Cr\u00e9er ce fichier dans un premier temps avec {.fn edstr_config},
                  ou bien indiquer un r\u00e9pertoire par d\u00e9faut avec {.field dest_dir}
                  et un nom de fichier par d\u00e9faut avec {.field dest_filename}"))

  }

}


cli_error_data <- \(data, fun) {

  cli_abort(c("Le fichier {.strong {data}} n'est pas charg\u00e9",
            "i" =
              "Cr\u00e9er/charger le fichier {.strong {data}} dans un premier
              temps avec {.fn edstr_{glue(fun)}} ou bien utiliser un autre fichier"))

}


cli_save <- \(data,
              config_file,
              config_save,
              rds = FALSE) {

  cli_progress_step("Enregistrement du fichier {.strong {config_file}}")

  assign(config_file, data, envir = rlang::global_env())

  if (rds) {

    readr::write_rds(x = get(config_file),
                     file = config_save)

  } else {

    save(list = config_file, file = config_save)

  }

  cli_progress_done()

  cli_alert_success("Fichier {.strong {config_file}} enregistr\u00e9 dans {.path {here(config_save)}}")
  cli_text("\n\n")
  cli_alert_info("{.strong Dimensions}")
  cli_ul()
    cli_li("{nrow(data)} documents")
    cli_li("{ncol(data)} variables")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}


cli_load <- \(dir,
              file,
              save,
              rds = FALSE,
              quiet = FALSE) {

  if (!quiet) {

  if (!file.exists(dir)) {

    cli_abort(c("Le fichier {.strong {file}} ne peut \u00eatre charg\u00e9 car le dossier
                {.path {here(dir)}} n'existe pas",
                "i" =
                  "Cr\u00e9er le fichier {.strong {file}} dans un premier temps avec
                  {.field load = FALSE} ou bien charger un autre fichier"))

  }

  if (!file.exists(save)) {

    cli_abort(c(" Le fichier {.strong {file}} n'est pas retrouv\u00e9 dans le dossier
                {.path {here(dir)}}",
                "i" =
                  "Cr\u00e9er le fichier {.strong {file}} dans un premier temps avec
                  {.field load = FALSE} ou bien changer le r\u00e9pertoire par d\u00e9faut
                  dans {.fn edstr_config} avec {.field dest_dir}"))

  }

  cli_progress_step("Chargement du fichier {.strong {file}}")

  if (rds) {

    assign(file, readRDS(save), envir = rlang::global_env())

  } else {

    load(save, envir = rlang::global_env())

  }

  cli_progress_done()

  cli_alert_success("Ficher {.strong {file}} charg\u00e9 depuis {.path {here(save)}}")
  cli_text("\n\n")
  cli_rule()

  } else {

    return(readRDS(save))

  }

}
