.extract_parse_concepts <- \(
  concepts,
  collapse,
  intersect,
  starts_with_only
) {
  if (length(concepts) > 1) {
    if (!is_named(concepts)) {
      cli_abort("Every concept must be named")
    }

    concepts <- as.list(concepts)
  } else if (!is.list(concepts)) {
    concepts <- if (is_named(concepts)) {
      as.list(concepts)
    } else {
      list("<concept>" = concepts)
    }
  }

  .clean_concepts_names <- \(x) {
    if (!is.list(x)) {
      return(x)
    }

    names(x) <- names(x) |> tolower() |> str_remove_all("[^a-z0-9]")

    map(x, .clean_concepts_names)
  }

  concepts <- .clean_concepts_names(concepts)

  if (collapse) {
    if (length(concepts) == 1) {
      cli_abort("Cannot collapse with a single concept")
    }

    concepts <- if (pluck_depth(concepts) > 2) {
      map(concepts, ~ paste(easy_flatten(.), collapse = "|"))
    } else {
      set_names(paste(concepts, collapse = "|"), "<concept>")
    }
  } else {
    concepts <- easy_flatten(concepts)
  }

  if (length(names(concepts)) == 1 && intersect) {
    cli_abort("Cannot intersect with a single concept")
  }

  regex_end <- if (starts_with_only) "\\S*$" else ""

  keys <- names(concepts)
  root <- unique(str_remove(keys, "_.+"))

  lst(
    keys = keys,
    root = root,
    names = imap(concepts, ~ if (is_named(.x)) names(.x) else .y),
    str = lst(
      comma = str_flatten_comma(root),
      inter = glue("[{paste(root, collapse = ' AND ')}]")
    ),
    regex = map(concepts, ~ glue("^({.}){regex_end}")),
    regex_df = tibble(
      concept_key = keys,
      concept_name = unlist(names),
      regex = unlist(map(concepts, ~ glue("^({.}){regex_end}")))
    )
  )
}

.extract_check_ids <- \(
  data,
  sample,
  text_input,
  id,
  group
) {
  which_key <- check_id_key(data = data, exclude = text_input, error = FALSE)

  if (is.null(id)) {
    id <- check_id_key(data = data, exclude = text_input)
  } else if (!(id %in% which_key)) {
    rlang::arg_match(id, which_key)
  }

  which_group <- check_id_group(data = data, id = group)

  if (is.null(group)) {
    group <- "id_group"

    data <- mutate(
      .data = data,
      !!group := row_number(),
      .after = all_of(id)
    )
  }

  nrow_init <- nrow(data)

  if (!is.null(sample)) {
    if (sample > nrow_init) {
      cli_abort(
        "{.arg sample} ({sample}) exceeds number of rows in {.arg data} ({nrow_init})"
      )
    }
    data <- data[sample(nrow_init, sample), ]
  }

  list(
    data = data,
    id = id,
    group = group,
    which_group = which_group,
    nrow_init = nrow_init
  )
}

.extract_format_text <- \(
  data,
  text_input,
  id,
  group,
  ano_hash,
  ano_hide
) {
  if (!is.null(ano_hash) || !is.null(ano_hide)) {
    data <- easy_ano(
      x = data,
      to_hash = ano_hash,
      to_hide = ano_hide
    )
  }

  easy_format <- \(text) {
    format_text <- regex("(?<=\">).+(?=</p>)")
    format_tags <- regex("</?[a-z]+/?>")

    text |>
      str_extract_all(format_text) |>
      map_chr(
        ~ . |>
          str_replace_all(format_tags, " ") |>
          paste(collapse = " ")
      ) |>
      str_squish() |>
      stri_trans_general("Latin-ASCII")
  }

  mutate(
    .data = data[c(id, group, text_input)],
    !!text_input := easy_format(.data[[text_input]])
  )
}

.extract_tokenize <- \(data_token, text_input, token) {
  easy_tokenize <- \(data, text, n, filter) {
    .data_ngram <- unnest_tokens(
      tbl = data,
      output = {{ text }},
      input = {{ text }},
      token = "ngrams",
      n = n
    )

    .data_ngram <- filter(
      .data_ngram,
      stri_detect_regex({{ text }}, filter)
    )

    .data_ngram
  }

  tokenize_fun <- \(n) {
    easy_tokenize(
      data = data_token,
      text = !!text_input,
      n = n,
      filter = "[:alpha:]"
    )
  }

  map(token, tokenize_fun)
}

.extract_mismatch <- \(
  data,
  data_match,
  data_match_init,
  data_regex_match,
  id,
  group,
  text_input,
  mismatch_data
) {
  data_id_mismatch_base <- data[c(id, group)]

  data_id_mismatch <- if (mismatch_data) {
    data_id_mismatch_base |> filter(!.data[[id]] %in% data_match_init[[id]])
  } else {
    data_id_mismatch_base |> filter(.data[[id]] %in% NA_character_)
  }

  .conv_fun <- \(x) {
    x |>
      stri_trans_general("Latin-ASCII") |>
      tolower() |>
      str_replace_all(c("-(<br/>)?|-?<br/>" = " ", "\\s+" = " "))
  }

  data_regex_mismatch <-
    data_match |>
    select(all_of(id), "concept", match = !!text_input) |>
    mutate(match = .conv_fun(.data$match)) |>
    anti_join(
      y = data_regex_match |> mutate(match = .conv_fun(.data$match)),
      by = c(id, "concept", "match")
    )

  lst(
    id = data_id_mismatch,
    regex = data_regex_mismatch
  )
}

.extract_results <- \(
  data_match_df,
  data_id,
  data_regex_list,
  data_regex_str,
  concepts_root,
  id,
  group,
  text_input
) {
  extract_data <- mutate(
    .data = data_match_df,
    extract = .data[[text_input]] |>
      str_extract_all(data_regex_str) |>
      map_chr(paste, collapse = " ; ")
  )

  extract_concept_name <-
    data_regex_list |>
    imap(
      ~ data_match_df |>
        mutate(
          extract = str_extract(.data[[text_input]], .x),
          concept = .y
        ) |>
        drop_na(extract) |>
        select(all_of(c(id, group)), "concept")
    ) |>
    list_rbind() |>
    nest(concept = "concept") |>
    mutate(concept = map_chr(.data$concept, ~ str_flatten(unlist(.), " ; ")))

  extract_concept_dummy <-
    data_id |>
    distinct(pick(all_of(c(id, group))), .data$concept_key) |>
    pivot_wider(
      names_from = "concept_key",
      values_from = "concept_key"
    ) |>
    mutate(across(matches(concepts_root), ~ ifelse(is.na(.), 0, 1)))

  lst(
    data = extract_data,
    concept_name = extract_concept_name,
    concept_dummy = extract_concept_dummy
  ) |>
    reduce(inner_join, by = c(id, group)) |>
    relocate("concept", "extract", .after = last_col()) |>
    arrange(pick(all_of(c(group, id)))) |>
    rownames_to_column("n")
}

.extract_summary <- \(
  data_match,
  data_match_exclus,
  data_id,
  data_count,
  id,
  group,
  params
) {
  set_summary <- \(var) {
    list(
      total = data_match,
      exclus_auto = data_match_exclus$auto,
      exclus_manual = data_match_exclus$manual,
      final = data_id,
      distinct = data_count
    ) |>
      set_names(~ glue("match_{.}")) |>
      imap(~ .x |> count(pick(all_of(var)), sort = TRUE, name = .y))
  }

  summary_concept <- list(
    match = set_summary("concept"),
    id = imap(
      list(id, group),
      ~ summarise(
        data_id,
        !!. := n_distinct(.data[[.]]),
        .by = "concept"
      )
    )
  )

  list(
    token = set_summary("token"),
    concept = list_flatten(summary_concept)
  ) |>
    imap(~ reduce(., left_join, by = .y)) |>
    append(list(params = params))
}
