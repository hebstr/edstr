.extract_match_source <- \(
  data_match_df, data_count, text_input, id, regex_replace
) {

  regex_replace_arg <- regex_replace

  regex_replace <- c(
    "a" = "[aàâæ]",
    "e" = "[eéèêëœ]",
    "i" = "[iîï]",
    "o" = "[oôœ]",
    "u" = "[uùûü]",
    "\\s" = "(?:<br/>)?[\\\\s\\\\-']+(?:<br/>)?"
  ) |>
    append(regex_replace) |>
    regex(multiline = TRUE)

  regex_wrap <- "(?i)\\b({x})\\b"

  regex_replace_df <- tibble(
    pattern = names(as.list(regex_replace)),
    replace = regex_replace
  )

  data_regex_replace <-
  data_count |>
    arrange(concept) |>
    mutate(!!text_input := str_replace_all(.data[[text_input]], regex_replace))

  data_regex_str <- glue(
    regex_wrap,
    x = data_regex_replace |> pull(text_input) |> paste(collapse = "|")
  )

  data_regex_df <-
  data_regex_replace |>
    nest(!!text_input := all_of(text_input), .by = concept) |>
    mutate(
      !!text_input := map_chr(.data[[text_input]], ~ str_flatten(unlist(.), "|")),
      !!text_input := glue(regex_wrap, x = .data[[text_input]])
    )

  data_regex_list <-
  data_regex_df[[text_input]] |>
    as.list() |>
    set_names(data_regex_df$concept)

  data_regex_match <- imap(
    data_regex_list,
    ~ view_output(
      data = data_match_df,
      text_input = text_input,
      pattern = .x,
      id = id
    ) |>
      pluck("match") |>
      mutate(concept = .y, .before = match)
  ) |>
    list_rbind()

  data_regex_count <- data_regex_match |> count(concept, match, sort = TRUE)

  list(
    regex_replace_arg = regex_replace_arg,
    regex_replace_df = regex_replace_df,
    data_regex_df = data_regex_df,
    data_regex_list = data_regex_list,
    data_regex_match = data_regex_match,
    data_regex_count = data_regex_count,
    data_regex_str = data_regex_str
  )

}
