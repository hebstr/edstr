check_config <- \(suffix = NULL) {
  id <- "edstr_dirname"

  .dirname <- getOption(id)
  .filename <- getOption("edstr_filename")

  if (is.null(.dirname) || is.null(.filename)) {
    cli_abort(
      message = c(
        "{.field edstr_dirname} or {.field edstr_filename} is not set",
        "i" = "Run {.fn edstr_config} first"
      ),
      call = rlang::caller_env()
    )
  } else {
    lst(
      dir = .dirname,
      file = str_glue("{.filename}_{suffix}"),
      save = fs::path(dir, file, ext = "rds")
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
      "{.arg {arg}} must be a {.cls {class}} object, not {.cls {class(x)}}",
      call = call
    )
  }

  invisible(x)
}

check_replace <- \(
  replace,
  arg = rlang::caller_arg(replace),
  call = rlang::caller_env()
) {
  ok <- is.character(replace) || is.list(replace)

  if (!ok) {
    cli_abort(
      "{.arg {arg}} must be a named character vector or a list of named character vectors",
      call = call
    )
  }

  elements <- if (is.list(replace)) replace else list(replace)

  bad <- !vapply(elements, \(x) is.character(x) && is_named(x), logical(1))

  if (any(bad)) {
    cli_abort(
      "Every element of {.arg {arg}} must be a named character vector (names = patterns, values = replacements)",
      call = call
    )
  }

  invisible(replace)
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
          "{.arg id}: no primary key found in {.strong {filename}}",
          "i" = "{.strong {filename}} must contain at least one column with unique, non-missing values"
        ),
        call = rlang::caller_env()
      )
    } else if (error) {
      str_id_key <- str_flatten_comma(id_key)

      cli_abort(
        message = c(
          "{.arg id}: multiple candidate primary keys found in {.strong {filename}}",
          "i" = "Choose one of: {str_id_key}"
        ),
        call = rlang::caller_env()
      )
    } else {
      return(invisible(id_key))
    }
  }

  invisible(id_key)
}

check_id_group <- \(
  data,
  id,
  arg = rlang::caller_arg(id),
  call = rlang::caller_env()
) {
  if (is.null(id)) {
    return(NULL)
  }

  if (!(id %in% names(data))) {
    cli_abort(
      message = "{.arg {arg}}: column {.strong {id}} not found",
      call = call
    )
  }

  if (anyNA(data[[id]])) {
    cli_abort(
      message = c(
        "{.arg {arg}}: {.strong {id}} contains missing values",
        "i" = "Group on a column with no missing values"
      ),
      call = call
    )
  }

  invisible(id)
}
