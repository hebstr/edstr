#' Title
#'
#' @param data
#' @param split
#' @param split_br
#' @param replace
#' @param n_min
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_split <- \(data,
                 split,
                 split_br,
                 replace,
                 n_min,
                 filter) {

  cli::cli_progress_step("step 1: extract")

  edstr_view(data = data,
             str = split,
             id = "id_sej")

  cli::cli_progress_step("step 2: filter")

  .split <-
  test_view |>
    purrr::pluck("all_matches") |>
    dplyr::mutate(match = match |> stringr::str_replace_all(replace))

  if (split_br) {

    .split <- .split |> tidyr::separate_rows(match, sep = "<br/>|(?<=:)\\s*")

  }

  .split <-
  .split |>
    dplyr::count(match, sort = TRUE) |>
    dplyr::filter(n > n_min,
                  stringr::str_detect(match, filter[1]),
                  !stringr::str_detect(match, filter[2]),
                  !duplicated(match))

  cli::cli_progress_done()

  assign(".split", .split, envir = .GlobalEnv)

}
