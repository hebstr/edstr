#' Explorer
#'
#' @param data data
#' @param text_input text_input
#' @param id id
#' @param replace replace
#' @param pattern pattern
#' @param ngrams ngrams
#' @param ... ...
#'
#' @return value
#' @export
#'
#' @examples "example"
#'
edstr_view <- \(
  data,
  text_input = getOption("edstr_text"),
  id = check_id_key(data = data, exclude = text_input),
  replace = NULL,
  pattern,
  ngrams = 1,
  ...
) {

  check_class(data, "data.frame")
  check_class(text_input, "character")
  check_class(pattern, "character")

  cli_h1("edstr_view"); br()

  which_key <- check_id_key(
    data = data,
    exclude = text_input,
    error = FALSE
  )

  if (!(id %in% which_key)) rlang::arg_match(id, which_key)

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

  br(); cli_rule(); br()

  match_id <- n_distinct(data_view$match[[id]])

  cli_p_match <- label_pct(match_id / nrow(data))

  toc()

  br(); cli_alert_info("{.strong Documents :} {nrow(data)} {id}"); br()

  cli_alert_info("{.strong Correspondances}")
  cli_ul()
  cli_ul()
    cli_li("Totales : {nrow(data_view$match)} parmi {match_id} {id} ({cli_p_match} {id})")
    cli_li("Distinctes : {nrow(data_view$count)}")
    cli_end()

  br(); cli_rule(); br()

  return(invisible(data_view))

}
