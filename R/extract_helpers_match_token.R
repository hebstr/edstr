.extract_match_token <- \(
  data,
  data_token,
  token,
  text_input,
  concepts_list,
  id,
  group,
  intersect
) {
  token_extract <- \(x, key, n) {
    occ <- data_token[[key]]

    vocab <- .timed_sub(
      glue("  match n{n}: vocab"),
      distinct(occ, pick(all_of(text_input)))
    )

    hits <- .timed_sub(
      glue("  match n{n}: K ICU scans"),
      imap(
        x,
        \(regex, concept_key) {
          vocab |>
            mutate(match = str_extract(.data[[text_input]], regex)) |>
            filter(!is.na(.data$match)) |>
            mutate(concept_hit = concept_key)
        }
      ) |>
        list_rbind()
    )

    .timed_sub(
      glue("  match n{n}: join+reshape"),
      occ |>
        inner_join(hits, by = text_input, relationship = "many-to-many") |>
        mutate(
          !!id := .data[[id]],
          !!group := .data[[group]],
          concept = .data$concept_hit,
          !!text_input := .data$match,
          .before = all_of(text_input),
          .keep = "none"
        ) |>
        drop_na() |>
        # arrange() is stable, so sorting on the concept factor alone restores
        # concept-major order and leaves each concept's rows in occurrence order
        arrange(factor(.data$concept, levels = names(x)))
    )
  }

  data_token_list <- imap(
    token,
    # keyed on the n-gram table's own name, since `token` holds sizes, not positions
    \(n, key) token_extract(concepts_list$regex, key, n)
  )

  data_token_match <- .timed_sub(
    "  match: counts",
    map(
      data_token_list,
      ~ count(
        x = .,
        .data$concept,
        pick(all_of(text_input)),
        name = "match",
        sort = TRUE
      )
    )
  )

  data_match_init <- imap(
    data_token_list,
    ~ mutate(.x, token = .y)
  ) |>
    list_rbind()

  .concepts_id <- map(
    set_names(concepts_list$root),
    ~ data_match_init |>
      distinct(pick(all_of(c(id, group))), .data$concept) |>
      pivot_wider(
        names_from = "concept",
        values_from = "concept"
      ) |>
      filter(if_any(
        matches(paste0("^", ., "(_|$)")) & !all_of(c(id, group)),
        ~ !is.na(.)
      )) |>
      select(all_of(c(id, group)))
  )

  match_id <- if (intersect) {
    reduce(.concepts_id, inner_join, by = c(id, group))
  } else {
    data_match_init[id]
  }

  # hoisted out of the mask below: a bare `id` there resolves to the column of
  # that name, not to the argument holding it
  match_ids <- match_id[[id]]

  data_match <-
    data_match_init |>
    rename(concept_key = "concept") |>
    mutate(
      concept = map_chr(.data$concept_key, ~ concepts_list$names[[.]]),
      .after = "concept_key"
    ) |>
    filter(.data[[id]] %in% match_ids)

  if (nrow(data_match) == 0) {
    abort_intersect <- if (intersect) " at intersection" else ""

    cli_abort("{.strong No matches found{abort_intersect}}")
  }

  match_init_ids <- data_match_init[[id]]
  match_final_ids <- data_match[[id]]

  data_match_init_df <-
    data |>
    select(-all_of(text_input)) |>
    filter(.data[[id]] %in% match_init_ids)

  data_match_df <- data |> filter(.data[[id]] %in% match_final_ids)

  lst(
    data_match = data_match,
    data_match_init = data_match_init,
    data_match_init_df = data_match_init_df,
    data_match_df = data_match_df,
    data_token_match = data_token_match,
    match_id = match_id
  )
}
