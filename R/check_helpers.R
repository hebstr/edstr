check_config <- \(suffix = NULL) {

  id <- "edstr_dirname"

  .dirname <- getOption(id)
  .filename <- getOption("edstr_filename")

  if (is.null(.dirname)) {

    cli_abort(
      message = c(
        "{.field {id}} n'est pas configur\u00e9",
        "i" = "D\u00e9finir un r\u00e9pertoire et un nom de fichier avec {.fn edstr_config}"
      ),
      call = rlang::caller_env()
    )

  } else {

    lst(
      dir = .dirname,
      file = str_glue("{.filename}_{suffix}"),
      save = str_glue("{dir}/{file}.rds")
    )

  }

}

check_class <- \(
  x,
  class,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {

  if (!inherits(x, class)) {

    cli_abort(
      "{.arg {arg}} doit \u00eatre un objet de type
      {.cls {class}}, pas {.cls {class(x)}}.",
      call = call
    )

  }

  invisible(x)

}

check_id_key <- \(data, exclude, error = TRUE) {

  filename <- enexpr(data)

  id_key <-
  data |>
    select(-all_of(exclude)) |>
    keep(~ anyDuplicated(.) == 0L && !anyNA(.)) |>
    names()

  if (length(id_key) != 1) {

    if (length(id_key) < 1) {

      cli_abort(
        message = c(
          "{.arg id} : aucune cl\u00e9 primaire identifi\u00e9e dans {.strong {filename}}",
          "i" = "{.strong {filename}} doit contenir au moins un identifiant unique et sans valeur manquante"
        ),
        call = rlang::caller_env()
      )

    } else if (error) {

      str_id_key <- str_flatten_comma(id_key)

      cli_abort(
        message = c(
          "{.arg id} : plusieurs cl\u00e9s primaires potentielles identifi\u00e9es dans {.strong {filename}}",
          "i" = "Choisir un identifiant parmi : {str_id_key}"
        ),
        call = rlang::caller_env()
      )

    } else {

      return(invisible(id_key))

    }

  }

  return(invisible(id_key))

}

check_id_group <- \(
  data,
  id,
  arg = rlang::caller_arg(id),
  call = rlang::caller_env()
) {

  if (is.null(id)) return(NULL)

  if (!(id %in% names(data))) {

    cli_abort(
      message = "{.arg {arg}}: la colonne {.strong {id}} n'existe pas",
      call = call
    )

  }

  if (anyNA(data[[id]])) {

    cli_abort(
      message = c(
        "{.arg {arg}}: {.strong {id}} contient au moins une valeur manquante",
        "i" = "Grouper sur un identifiant sans valeur manquante"
      ),
      call = call
    )

  }

  return(invisible(id))

}
