.extract_print <- \(concepts,
                    data_id,
                    data_count,
                    concept,
                    group) {

  ngrams_max <- n_distinct(data_count$ngrams)

  .print <-
  list(regex =
         tibble(concept = names(concepts),
                regex = map_chr(concepts, ~ glue("^({.})$"))),
       concept =
         list(match = data_id,
              distinct = data_count) |>
           imap(~ .x |> count(concept, name = .y)),
       group =
         data_id |>
           summarise(!!group := n_distinct(get(group)),
                     .by = concept)) |>
    list_flatten() |>
    reduce(left_join, by = "concept")


  if (ngrams_max > 1) {

    data_plot <-
    ggplot(data_count) +
      geom_bar(mapping = aes(x = ngrams, fill = concept),
               stat = "count") +
      scale_x_continuous(n.breaks = ngrams_max)

    print(data_plot)

  }

  print(list(concepts = .print))

}
