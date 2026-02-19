cli_save <- \(data, config_file, config_save) {

  cli_progress_step("Enregistrement du fichier {.strong {config_file}}")

  saveRDS(data, file = config_save)

  cli_progress_done()

  cli_alert_success("Fichier {.strong {config_file}} enregistr\u00e9 dans {.strong {.path {config_save}}}")
  br()

  cli_alert_info("{.strong Dimensions}")
  cli_ul()
    cli_ul()
    cli_li("{nrow(data)} documents")
    cli_li("{ncol(data)} variables")
    cli_end()

  br(); cli_rule()

  return(invisible(data))

}

cli_load <- \(dir, file, save) {

  cli_progress_step("Chargement du fichier {.strong {file}}")

  .load <- readRDS(save)

  cli_progress_done()

  cli_alert_success("Ficher {.strong {file}} charg\u00e9 depuis {.strong {.path {save}}}")

  br(); cli_rule()

  return(invisible(.load))

}

cli_check <- \(config_file, fun_save, fun_load) {

  br(); cli_alert_warning("Le fichier {.strong {config_file}} existe d\u00e9ja.")

  choix <- menu(
    choices = c(
      "Charger le fichier existant",
      "\u00c9craser le fichier existant",
      "Annuler"
    ),
    title = NULL
  )

  if (choix == 1) fun_load()

    else if (choix == 2) fun_save()

      else cli_abort("Op\u00e9ration annul\u00e9e.")

}
