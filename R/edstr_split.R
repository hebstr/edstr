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
edstr_split <- \(data = glue::glue("{with(config, file)}_clean"),
                 split,
                 split_br,
                 replace,
                 n_min,
                 filter,
                 config = get(.config_name)) {

  if (is.character(data)) data <- get(data)
  if (is.character(config)) config <- get(config)

  cli::cli_progress_step("step 1: extract")

  edstr_view(data = data,
             str = split,
             id = "id_sej")

  config_view <- get(glue::glue("{with(config, file)}_view"))

  cli::cli_progress_step("step 2: filter")

  .split <-
  config_view |>
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
