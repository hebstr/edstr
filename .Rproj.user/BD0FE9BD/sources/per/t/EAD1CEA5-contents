#' Title
#'
#' @param ... One or more character vector
#'
#' @return A character vector
#' @export
#'
#' @examples
#'
str_u <- \(...) {

stringr::str_c(unlist(c(...)), collapse = "|")

}


#' Title
#'
#' @param ... A character vector
#' @param replace Replacement pattern. A character vector.
#'
#' @return A named character vector
#' @export
#'
#' @examples
#'
easy_replace <- \(..., replace) {

  str_list <-
  purrr::map(c(...),
             ~ rlang::list2('{glue("<p>.*({.}).*</p>")}' := replace) |>
               unlist())

  replace_list <-
    rlang::list2("(\n*{replace})+\n*" := glue::glue("\n\n\n{col_br_red({replace})}\n\n\n"))

  unlist(append(str_list, replace_list))

}
