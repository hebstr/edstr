#' Explorer
#'
#' @param data data
#' @param text_input text_input
#' @param replace replace
#' @param pattern pattern
#' @param case_sensitive case_sensitive
#' @param starts_with_only starts_with_only
#' @param token_max token_max
#' @param id id
#' @param n_text_output n_text_output
#' @param print_text print_text
#' @param print_count print_count
#' @param ... ...
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_view <- \(
  data,
  text_input = getOption("edstr_text"),
  replace = NULL,
  pattern = NULL,
  case_sensitive = FALSE,
  starts_with_only = FALSE,
  token_max = NULL,
  id = NULL,
  n_text_output = 5,
  print_text = FALSE,
  print_count = TRUE,
  ...
) {

  cli_h1("edstr_view")
  cli_text("\n\n")

  cli_progress_step("Creating ???")

### REPLACE --------------------------------------------------------------------

  if (!is.null(replace)) {

    if (!is.list(replace)) replace <- list(replace)

    data <- mutate(
      .data = data,
      !!text_input := reduce(
        replace,
        str_replace_all,
        .init = .data[[text_input]]
      )
    )

  }

### EXTRACT --------------------------------------------------------------------

  enstr <- \(x, y = x) pattern <- glue("{x}{pattern}{y}")

  if (!is.null(token_max)) {

    pattern <- enstr('', '(\\w*\\s*((-|/)\\s*)?\\w+)')
    pattern <- glue("{pattern}{{1,", token_max - 1, "}}")

  }

  if (!case_sensitive) pattern <- enstr("(?i)", "")

  if (starts_with_only) pattern <- enstr("", "\\S*")

  data_match <-
  data |>
    mutate(match = str_extract_all(data[[text_input]], pattern)) |>
    unnest(match) |>
    select(id, match)

  match_id <- n_distinct(data_match[[id]])

  if (nrow(data_match) == 0) cli_abort("{.strong Aucune correspondance}")

  data_count <- data_match |> count(match, sort = TRUE)

### OUTPUT SAMPLE ---------------------------------------------------------------

  if (n_text_output > 0) {

    n_text_output_max <- if (match_id > n_text_output) "(max)" else NULL

    data_text_output <-
    data |>
      filter(.data[[id]] %in% data_match[[id]]) |>
      slice(1:n_text_output) |>
      pull(text_input) |>
      str_view(pattern, ...)

  } else {

    data_text_output <- NULL

  }

  output <- list(
    text = data_text_output,
    count = data_count
  )

  if (print_text) print(output$text)
  if (print_count) print(output$count)

  cli_text("\n\n")
  cli_progress_done()

### CLI ------------------------------------------------------------------------

  cli_h1("edstr_view")
  cli_text("\n\n")

  cli_p_match <- label_percent(0.1)(match_id / nrow(data))

  cli_alert_info("{.strong Documents :} {nrow(data)} {id}")
  cli_text("\n\n")

  cli_alert_info("{.strong Correspondances}")
  cli_ul()
  cli_ul()
    cli_li("Totales : {nrow(data_match)} parmi {match_id} {id} ({cli_p_match} {id})")
    cli_li("Distinctes : {nrow(data_count)}")
    cli_end()

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  data_view <- list(
    match = data_match,
    count = data_count
  )

  return(invisible(data_view))

}
