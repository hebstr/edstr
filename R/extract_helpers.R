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

  if (anyDuplicated(keys)) {
    dupes <- unique(keys[duplicated(keys)])
    cli_abort(c(
      "Concept names collide after normalisation.",
      "x" = "These normalised keys are duplicated: {.val {dupes}}.",
      "i" = "Names are lowercased and stripped to {.code [a-z0-9]}; rename the concepts so they stay distinct."
    ))
  }

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
      regex = unlist(regex)
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
    format_text <- regex("\">(.+?)</p>", dotall = TRUE)
    format_tags <- regex("</?[a-z]+/?>")

    text |>
      str_match_all(format_text) |>
      map(~ .[, 2]) |>
      map_chr(
        ~ . |>
          str_replace_all(format_tags, " ") |>
          paste(collapse = " ")
      ) |>
      str_squish() |>
      stri_trans_general("Latin-ASCII")
  }

  data <- data[c(id, group, text_input)]
  raw <- data[[text_input]]
  formatted <- easy_format(raw)

  had_text <- !is.na(raw) & raw != ""
  dropped <- had_text & (is.na(formatted) | formatted == "")

  drops <- NULL
  if (any(dropped)) {
    strip_all <- \(x) {
      x |>
        str_remove_all(regex("<(style|script)[^>]*>.*?</\\1>", dotall = TRUE)) |>
        str_remove_all(regex("<head[^>]*>.*?</head>", dotall = TRUE)) |>
        str_remove_all(regex("<!--.*?-->", dotall = TRUE)) |>
        str_remove_all("<[^>]+>") |>
        str_replace_all("&#?[a-z0-9]+;", " ") |>
        str_squish()
    }

    dropped_idx <- which(dropped)
    has_body <- nzchar(strip_all(raw[dropped_idx]))

    drops <- list(
      empty_text = data[[id]][dropped_idx[!has_body]],
      outside_p = data[[id]][dropped_idx[has_body]]
    )
  }

  data[[text_input]] <- formatted
  attr(data, "format_drops") <- drops

  data
}

.extract_print_drops <- \(format_drops) {
  if (is.null(format_drops)) {
    return(invisible())
  }

  n_empty <- length(format_drops$empty_text)
  n_uncovered <- length(format_drops$outside_p)
  n_dropped <- n_empty + n_uncovered

  ul <- cli_ul()
  cli_li("{n_dropped} document{?s} produced no matchable text")

  ul_detail <- cli_ul()
  if (n_empty > 0) {
    cli_li(
      "{n_empty} with no recoverable text (no text once markup is stripped)"
    )
  }
  if (n_uncovered > 0) {
    cli_li(
      "{n_uncovered} with text outside blocks not captured"
    )
  }
  cli_end(ul_detail)
  cli_end(ul)
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

.extract_unmatched <- \(
  data,
  data_match,
  data_match_init,
  data_regex_match,
  id,
  group,
  text_input,
  unmatched_data,
  format_drops
) {
  unmatched_all <-
    data[c(id, group)] |>
    filter(!.data[[id]] %in% data_match_init[[id]])

  empty_ids <- format_drops$empty_text
  outside_ids <- format_drops$outside_p

  unmatched <- lst(
    no_concept = if (unmatched_data) {
      unmatched_all |> filter(!.data[[id]] %in% c(empty_ids, outside_ids))
    } else {
      unmatched_all |> slice(0)
    },
    empty_text = unmatched_all |> filter(.data[[id]] %in% empty_ids),
    outside_p = unmatched_all |> filter(.data[[id]] %in% outside_ids)
  )

  .conv_fun <- \(x) {
    x |>
      stri_trans_general("Latin-ASCII") |>
      tolower() |>
      str_replace_all(c("-(<br/>)?|-?<br/>" = " ", "\\s+" = " "))
  }

  mismatched <-
    data_match |>
    select(all_of(id), "concept", match = !!text_input) |>
    mutate(match = .conv_fun(.data$match)) |>
    anti_join(
      y = data_regex_match |> mutate(match = .conv_fun(.data$match)),
      by = c(id, "concept", "match")
    )

  lst(
    unmatched = unmatched,
    mismatched = mismatched
  )
}

.extract_results <- \(
  data_match_df,
  data_id,
  data_regex_match,
  data_regex_str,
  data_regex_prep,
  id,
  group
) {
  extract_data <- mutate(
    .data = data_match_df,
    extract = .re2_extract_prepared(data_regex_prep, data_regex_str) |>
      map_chr(paste, collapse = " ; ")
  )

  extract_concept_name <-
    data_regex_match |>
    distinct(pick(all_of(c(id, "concept")))) |>
    left_join(
      distinct(data_match_df, pick(all_of(c(id, group)))),
      by = id
    ) |>
    nest(concept = "concept") |>
    mutate(concept = map_chr(.data$concept, ~ str_flatten(unlist(.), " ; ")))

  extract_concept_dummy <-
    data_id |>
    distinct(pick(all_of(c(id, group))), .data$concept_key) |>
    pivot_wider(
      names_from = "concept_key",
      values_from = "concept_key"
    ) |>
    mutate(across(!all_of(c(id, group)), ~ ifelse(is.na(.), 0, 1)))

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
