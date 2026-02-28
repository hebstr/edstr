.clean_save <- \(
  data,
  text,
  replace,
  config
) {

  check_class(data, "data.frame")
  check_class(text, "character")

  cli_h1("edstr_clean"); br()

  cli_progress_step("Nettoyage du texte ({.strong {text}})")

  data_clean <- easy_replace(
    data = data,
    pattern = replace,
    text = text
  )

  cli_progress_done()

  cli_save(
    data = data_clean,
    config_file = config$file,
    config_save = config$save
  )

}

.clean_load <- \(config) {

  cli_h1("edstr_clean"); br()

  cli_load(
    dir = config$dir,
    file = config$file,
    save = config$save
  )

}

#' Nettoyer les données textuelles
#'
#' Applique des remplacements (regex) sur une colonne de texte via
#' [stringr::str_replace_all()]. Le résultat est enregistré en `.rds` dans le
#' répertoire défini par [edstr_config()]. Si le fichier existe déjà,
#' l'utilisateur choisit entre charger ou écraser (voir option
#' `edstr_overwrite`).
#'
#' @param data Un dataframe/tibble contenant la colonne texte à nettoyer.
#' @param text Nom de la colonne de texte en character. Par défaut,
#'   `getOption("edstr_text")`.
#' @param replace Vecteur nommé ou liste de vecteurs nommés de remplacements,
#'   passés à [stringr::str_replace_all()].
#'
#' @returns Le tibble nettoyé
#' @export
#'
#' @examples
#' edstr_config(
#'   edstr_dirname = "_extra/collect/data",
#'   edstr_filename = "avc",
#'   edstr_text = "texte",
#'   edstr_overwrite = FALSE
#' )
#'
#' df <- tibble::tibble(texte = c("la lapin mange des carrotes"))
#'
#' df_clean <- edstr_clean(
#'   data = df,
#'   replace = c("lapin" = "renard")
#' )
#'
#' df_clean
#'
edstr_clean <- \(
  data,
  text = getOption("edstr_text"),
  replace
) {

  config <- check_config("clean")

  fun_save <- \() .clean_save(data, text, replace, config)

  if (fs::file_exists(config$save)) {

    cli_check(
      config_file = config$file,
      fun_save = fun_save,
      fun_load = \() .clean_load(config)
    )

  } else {

    fun_save()

  }

}
