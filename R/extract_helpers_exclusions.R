.extract_exclusions <- \(
  data_match, text_input, id, group, exclus_manual, exclus_auto_escape
) {

  data_match$token <- as.numeric(str_remove(data_match$token, "n"))

  if (!is.null(exclus_auto_escape)) {

    data_match <- filter(
      data_match,
      !str_detect(.data[[text_input]], exclus_auto_escape)
    )

  }

  set_exclus_auto <- \(regex, name) {

    unique(data_match[[text_input]]) |>
      map(
        ~ data_match |>
          filter(str_detect(.data[[text_input]], glue(regex))) |>
          mutate(!!name := .)
      ) |>
      list_rbind()

  }

  str_detect_safe <- \(string, pattern) {

    if (is.null(pattern) || is.na(pattern)) {

      rep(FALSE, length(string))

    } else {

      str_detect(string, pattern)

    }

  }

  data_match_exclus <- list(
    auto =
      list(start = "^{.}\\s", end = "\\s{.}$", start_end = "{.}.+{.}$") |>
        imap(set_exclus_auto) |>
        reduce(full_join, by = names(data_match)) |>
        filter(.data$token > 10),
    manual = filter(
      data_match,
      str_detect_safe(.data[[text_input]], exclus_manual)
    )
  ) |>
    imap(~ .x |> mutate(mode = .y, .before = everything()))

  .match_exclus <-
  data_match_exclus |>
    list_rbind() |>
    pull(text_input) |>
    unique()

  data_match_final <- list(
    keep = filter(data_match, !.data[[text_input]] %in% .match_exclus),
    drop = filter(data_match, .data[[text_input]] %in% .match_exclus)
  )

  data_count <- map(
    data_match_final,
    ~ list2(
      match = .,
      !!id := distinct(., pick(-group)),
      !!group := distinct(., pick(-id))
    ) |>
      imap(
        ~ count(
          x = .,
          .data$token, .data$concept, pick(text_input),
          name = .y,
          sort = TRUE
        )
      ) |>
      reduce(left_join, by = c("concept", text_input, "token"))
  )

  data_count_exclus <-
  data_match_exclus |>
    map(~ select(., -.env$id, -.env$group)) |>
    list_rbind() |>
    distinct() |>
    relocate(.data$token, .before = .data$concept) |>
    left_join(
      y = data_count$drop,
      by = c("token", "concept", text_input)
    )

  list(
    data_match = data_match,
    data_match_exclus = data_match_exclus,
    data_match_final = data_match_final,
    data_id = data_match_final$keep,
    data_count = data_count$keep,
    data_count_exclus = data_count_exclus
  )

}
