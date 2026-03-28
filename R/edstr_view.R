#' Explore text matches interactively
#'
#' Search for regex patterns in a text column and display matching tokens,
#' counts, and highlighted text. Unlike other pipeline functions, `edstr_view()`
#' does not save results — it is meant for iterating on patterns before
#' extraction.
#'
#' @param data `<data.frame>` The data to search.
#' @param text_input `<character(1)>` Name of the text column. Defaults to
#'   the `edstr_text` option set by [edstr_config()].
#' @param id `<character(1)>` Name of the unique identifier column. Auto-
#'   detected automatically if not provided.
#' @param replace A named character vector or list of named character vectors.
#'   Optional regex replacements applied to the text *before* matching
#'   (see [edstr_clean()] for details).
#' @param pattern `<character(1)>` Regex pattern to search for.
#' @param ngrams `<integer(1)>` Number of tokens to capture after the
#'   initial match (default `1`). For example, `ngrams = 3` with
#'   `pattern = "diabete"` matches `"diabete type 2"`.
#' @param ... Additional arguments passed to [stringr::str_view()].
#'
#' @return Invisibly returns a list with three elements:
#'   \describe{
#'     \item{`match`}{A [tibble] of all matches with the `id` and `match`
#'       columns.}
#'     \item{`count`}{A [tibble] of distinct matches with their frequency.}
#'     \item{`text`}{Output of [stringr::str_view()] for visual inspection.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' edstr_view(
#'   data = df_clean,
#'   pattern = "diabete",
#'   ngrams = 3
#' )
#' }
#'
edstr_view <- \(
  data,
  text_input = getOption("edstr_text"),
  id = NULL,
  replace = NULL,
  pattern,
  ngrams = 1,
  ...
) {
  check_class(data, "data.frame")
  check_class(text_input, "character")
  check_class(pattern, "character")

  cli_h1("edstr_view")
  br()

  which_key <- check_id_key(
    data = data,
    exclude = text_input,
    error = FALSE
  )

  if (is.null(id)) {
    if (length(which_key) == 1) {
      id <- which_key
    } else {
      id <- check_id_key(data = data, exclude = text_input)
    }
  } else if (!(id %in% which_key)) {
    rlang::arg_match(id, which_key)
  }

  tic("Full steps")

  if (!is.null(replace)) {
    data <- easy_replace(
      data = data,
      pattern = replace,
      text = text_input
    )
  }

  data_view <- view_output(
    data = data,
    text_input = text_input,
    pattern = glue("{pattern}(?:[\\s\\-/]+\\w+){{0,{ngrams - 1}}}"),
    id = id,
    ...
  )

  print(data_view$count)

  br()
  cli_rule()
  br()

  match_id <- n_distinct(data_view$match[[id]])

  cli_p_match <- label_pct(match_id / nrow(data))

  toc()

  br()
  cli_alert_info("{.strong Documents:} {nrow(data)} {id}")
  br()

  cli_alert_info("{.strong Matches}")
  cli_ul()
  cli_ul()
  cli_li(
    "Total: {nrow(data_view$match)} across {match_id} {id} ({cli_p_match} {id})"
  )
  cli_li("Distinct: {nrow(data_view$count)}")
  cli_end()

  br()
  cli_rule()
  br()

  invisible(data_view)
}
