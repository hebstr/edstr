.extract_print <- \(str,
                    data_id,
                    data_count,
                    concept,
                    group_by) {

  print_query <-
  purrr::map2(.x = names(str),
              .y = seq(str),
              ~ rlang::list2(!!.x := glue::glue("^({hebstr::str_u(str[.y])})$"))) |>
    unlist()

  print_concept <-
  list(data_id, data_count) |>
    purrr::map(~ . |>
                 dplyr::group_by(concept) |>
                 dplyr::count() |>
                 dplyr::ungroup())

  print_group <-
  data_id |>
    dplyr::group_by(concept) |>
    dplyr::mutate(!!group_by := dplyr::n_distinct(get(group_by))) |>
    dplyr::ungroup() |>
    dplyr::distinct(concept, !!group_by := get(group_by))

  print_concept <-
  dplyr::inner_join(print_concept[[1]] |> dplyr::rename(match = n),
                    print_concept[[2]] |> dplyr::rename(distinct = n),
                    by = "concept") |>
    dplyr::inner_join(print_group, by = "concept")

  if (dplyr::n_distinct(data_count$ngrams) > 1) {

    data_plot <-
    data_count |>
      ggplot2::ggplot(ggplot2::aes(x = ngrams, y = n)) +
      ggplot2::geom_col(ggplot2::aes(fill = concept)) +
      ggplot2::labs(title = "nb matchs apres exclusion")

    print(data_plot)

  }

  print(list(query = print_query,
             concepts = print_concept))

}
