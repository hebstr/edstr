.extract_print <- \(concepts,
                    data_id,
                    data_count,
                    concept,
                    group) {

  ngrams_max <- n_distinct(data_count$ngrams)

  print_query <-
  map2(.x = names(concepts),
       .y = seq(concepts),
       ~ list2(!!.x := glue("^({str_u(concepts[.y])})$"))) |>
    unlist()

  print_concept <-
  list(data_id, data_count) |>
    map(~ . |>
          group_by(concept) |>
          count() |>
          ungroup())

  print_group <-
  data_id |>
    group_by(concept) |>
    mutate(!!group := n_distinct(get(group))) |>
    ungroup() |>
    distinct(concept, !!group := get(group))

  print_concept <-
  inner_join(print_concept[[1]] |> rename(match = n),
             print_concept[[2]] |> rename(distinct = n),
             by = "concept") |>
    inner_join(print_group, by = "concept")

  if (ngrams_max > 1) {

    data_plot <-
    ggplot(data_count) +
      geom_bar(mapping = aes(x = ngrams, fill = concept),
               stat = "count") +
      scale_x_continuous(n.breaks = ngrams_max)

    print(data_plot)

  }

  print(list(query = print_query,
             concepts = print_concept))

}
