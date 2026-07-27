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
      list(concepts = concepts)
    }
  }

  .clean_concepts_names <- \(x) {
    if (!is.null(names(x))) {
      names(x) <- names(x) |> tolower() |> str_remove_all("[^a-z0-9]")
    }

    if (!is.list(x)) {
      return(x)
    }

    map(x, .clean_concepts_names)
  }

  concepts <- .clean_concepts_names(concepts)

  if (collapse) {
    if (length(concepts) == 1) {
      cli_abort("Cannot collapse with a single concept")
    }

    # a root holding several patterns collapses within itself; a flat set of
    # single-pattern roots collapses into one. Depth cannot tell them apart:
    # both are depth 2 once `concepts` is a list.
    concepts <- if (any(lengths(concepts) > 1)) {
      map(concepts, ~ paste(unlist(.), collapse = "|"))
    } else {
      set_names(paste(unlist(concepts), collapse = "|"), "concepts")
    }
  } else {
    concepts <- easy_flatten(concepts)
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

  concept_names <- imap(concepts, ~ if (is_named(.x)) names(.x) else .y)

  flat_names <- unlist(concept_names, use.names = FALSE)

  if (anyDuplicated(flat_names)) {
    dupes <- unique(flat_names[duplicated(flat_names)])
    cli_abort(c(
      "Concept sub-names collide after normalisation.",
      "x" = "These normalised sub-names are duplicated: {.val {dupes}}.",
      "i" = "Sub-names label the {.code concept} column and group the source passes, so distinct concepts sharing one would merge; rename them so they stay distinct."
    ))
  }

  root <- unique(str_remove(keys, "_.+"))

  if (length(root) == 1 && intersect) {
    cli_abort("Cannot intersect with a single concept")
  }

  lst(
    keys = keys,
    root = root,
    names = concept_names,
    str = lst(
      comma = str_flatten_comma(root),
      inter = glue("[{paste(root, collapse = ' AND ')}]")
    ),
    regex = map(concepts, ~ glue("^({.}){regex_end}")),
    regex_df = tibble(
      concept_key = keys,
      concept_name = unlist(names, use.names = FALSE),
      regex = unlist(regex, use.names = FALSE)
    )
  )
}

.extract_check_ids <- \(
  data,
  sample,
  text_input,
  id,
  group,
  concept_keys
) {
  # every one of these is written by `.extract_results()`, silently overwriting
  # or mangling a user column of the same name
  reserved <- c("n", "concept", "extract", concept_keys)

  if (is.null(group)) {
    reserved <- c(reserved, "id_group")
  }

  clash <- intersect(names(data), reserved)

  if (length(clash) > 0) {
    cli_abort(c(
      "{.arg data} holds {length(clash)} column{?s} the output builds for itself.",
      "x" = "Reserved: {.val {clash}}.",
      "i" = "Rename {qty(length(clash))}{?it/them} in {.arg data}, rename the
             colliding concept, or pass {.arg group} to keep your own grouping
             column."
    ))
  }

  which_key <- check_id_key(data = data, exclude = text_input, error = FALSE)

  if (is.null(id)) {
    # a single candidate is already the answer; the re-scan exists only to raise
    # the ambiguity error, which lists the candidates
    id <- if (length(which_key) == 1) {
      which_key
    } else {
      check_id_key(data = data, exclude = text_input)
    }
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

.extract_anonymise <- \(
  data,
  id,
  group,
  text_input,
  ano_hash,
  ano_hide
) {
  if (is.null(ano_hash) && is.null(ano_hide)) {
    return(data)
  }

  patterns <- c(ano_hash, ano_hide)
  origin <- rep(
    c("ano_hash", "ano_hide"),
    c(length(ano_hash), length(ano_hide))
  )

  matched <- map(
    patterns,
    ~ str_subset(names(data), regex(.x, ignore_case = TRUE))
  )

  blank <- lengths(matched) == 0

  if (any(blank)) {
    cli_abort(c(
      "Every pattern in {.arg {unique(origin[blank])}} must match a column.",
      "x" = "{.val {patterns[blank]}} match{?es/} no column of {.arg data}.",
      "i" = "Column names are matched as case-insensitive regular expressions."
    ))
  }

  hit <- unique(unlist(matched))
  keys <- intersect(hit, c(id, group))

  if (length(keys) > 0) {
    on_key <- map_lgl(matched, ~ any(.x %in% c(id, group)))

    cli_abort(c(
      "{.arg {unique(origin[on_key])}} cannot target a key column.",
      "x" = "The pattern matches {.val {keys}}, which {?is/are} used to join
             the outputs back together.",
      "i" = "Anonymise the key before calling {.fn edstr_extract}."
    ))
  }

  if (text_input %in% hit) {
    on_text <- map_lgl(matched, ~ text_input %in% .x)

    cli_abort(c(
      "{.arg {unique(origin[on_text])}} cannot target the text column.",
      "x" = "The pattern matches {.val {text_input}}, the source every match is
             read from.",
      "i" = "The {.field extract} and {.field note} outputs quote that source
             verbatim; drop the column before calling {.fn edstr_extract} if it
             must not appear."
    ))
  }

  easy_ano(
    x = data,
    to_hash = ano_hash,
    to_hide = ano_hide
  )
}

.extract_format_text <- \(
  data,
  text_input,
  id,
  group
) {
  easy_format <- \(text) {
    format_text <- regex("\">(.+?)</p>", dotall = TRUE)
    format_tags <- regex("</?[a-z]+/?>")

    captured <- .timed_sub(
      "  format: html capture",
      text |>
        str_match_all(format_text) |>
        map(~ .[, 2])
    )

    stripped <- .timed_sub(
      "  format: strip+squish",
      captured |>
        map_chr(
          ~ . |>
            str_replace_all(format_tags, " ") |>
            paste(collapse = " ")
        ) |>
        str_squish()
    )

    # the same fold source matching uses, so a token and the folded source are
    # the same string by construction
    .timed_sub("  format: re2 fold", .re2_fold(stripped))
  }

  data <- data[c(id, group, text_input)]
  raw <- data[[text_input]]
  had_text <- !is.na(raw) & raw != ""

  formatted <- easy_format(raw)
  # `paste()` renders an NA capture as the string "NA", which would reach
  # tokenisation as a token on a document that has no source
  formatted[!had_text] <- ""

  dropped <- had_text & (is.na(formatted) | formatted == "")

  drops <- NULL
  if (any(dropped) || !all(had_text)) {
    strip_all <- \(x) {
      x |>
        str_remove_all(regex(
          "<(style|script)[^>]*>.*?</\\1>",
          dotall = TRUE
        )) |>
        str_remove_all(regex("<head[^>]*>.*?</head>", dotall = TRUE)) |>
        str_remove_all(regex("<!--.*?-->", dotall = TRUE)) |>
        str_remove_all("<[^>]+>") |>
        str_replace_all("&#?[a-z0-9]+;", " ") |>
        str_squish()
    }

    dropped_idx <- which(dropped)
    has_body <- nzchar(strip_all(raw[dropped_idx]))

    drops <- list(
      no_source = data[[id]][!had_text],
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

  n_no_source <- length(format_drops$no_source)
  n_empty <- length(format_drops$empty_text)
  n_uncovered <- length(format_drops$outside_p)
  n_dropped <- n_no_source + n_empty + n_uncovered

  ul <- cli_ul()
  cli_li("{n_dropped} document{?s} produced no matchable text")

  ul_detail <- cli_ul()
  if (n_no_source > 0) {
    cli_li(
      "{n_no_source} with an empty or missing source"
    )
  }
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
  # a per-document row index, carried through the unnest and dropped before the
  # output, so document boundaries are known without threading `id` in
  doc_key <- ".__edstr_doc"
  data_token[[doc_key]] <- seq_len(nrow(data_token))

  words <- .timed_sub(
    "  tokenise: unnest n1",
    unnest_tokens(
      tbl = data_token,
      output = !!rlang::sym(text_input),
      input = !!rlang::sym(text_input),
      token = "ngrams",
      n = 1
    )
  )

  w <- words[[text_input]]
  doc <- words[[doc_key]]
  words[[doc_key]] <- NULL
  max_n <- max(token)

  # higher-order n-grams are the n=1 stream slid within each document: a gram of
  # size k pastes w[i..i+k-1], kept only where the span stays in one document
  # (doc[i] == doc[i+k-1]). This reproduces unnest_tokens(n = k) exactly at a
  # single tokenisation (verified against the per-n unnest over the corpus), so
  # the n>=2 passes no longer re-tokenise the source.
  leads <- .timed_sub(
    "  tokenise: leads",
    map(seq_len(max_n - 1L), \(k) {
      same_doc <- doc == lead(doc, k)
      same_doc[is.na(same_doc)] <- FALSE

      list(word = lead(w, k), same_doc = same_doc)
    })
  )

  build <- \(n) {
    .timed_sub(glue("  tokenise n{n}: slide+filter"), {
      if (n == 1L) {
        # the n=1 stream itself; no slide, so filter it directly
        filter(words, stri_detect_regex(.data[[text_input]], "[:alpha:]"))
      } else {
        gram <- do.call(
          stri_c,
          c(list(w), map(leads[seq_len(n - 1L)], "word"), list(sep = " "))
        )
        keep <- leads[[n - 1L]]$same_doc

        out <- words
        out[[text_input]] <- gram

        # one dplyr pass: same-document span AND an alphabetic character. `dplyr`
        # filtering is far cheaper here than base row-subsetting of a wide tibble.
        # `stri_c` yields NA on the boundary rows `keep` already drops, so the
        # kept output matches base `paste` exactly
        filter(out, keep & stri_detect_regex(.data[[text_input]], "[:alpha:]"))
      }
    })
  }

  map(token, build)
}

.extract_unmatched <- \(
  data,
  data_id,
  data_regex_match,
  id,
  group,
  text_input,
  unmatched_data,
  format_drops
) {
  # the confirmed-match set, after the intersection, the exclusions and the
  # source confirmation: a document matching some roots but not all, or whose
  # every match is excluded or unconfirmed in the source, is a non-case, and
  # leaving it out of every bucket understates the denominator `n_no_concept` is
  # documented to be. `mismatched` cannot stand in for that bucket: it is per
  # match, so a partially confirmed document is in it and in `extract` at once
  match_ids <- intersect(data_id[[id]], data_regex_match[[id]])

  unmatched_all <-
    data[c(id, group)] |>
    filter(!.data[[id]] %in% match_ids)

  no_source_ids <- format_drops$no_source
  empty_ids <- format_drops$empty_text
  outside_ids <- format_drops$outside_p

  no_concept_all <- unmatched_all |>
    filter(!.data[[id]] %in% c(no_source_ids, empty_ids, outside_ids))

  unmatched <- lst(
    n_no_concept = nrow(no_concept_all),
    no_concept = if (unmatched_data) {
      no_concept_all
    } else {
      no_concept_all |> slice(0)
    },
    no_source = unmatched_all |> filter(.data[[id]] %in% no_source_ids),
    empty_text = unmatched_all |> filter(.data[[id]] %in% empty_ids),
    outside_p = unmatched_all |> filter(.data[[id]] %in% outside_ids)
  )

  mismatched <-
    data_id |>
    select(all_of(id), "concept", match = !!text_input) |>
    mutate(match = .extract_source_key(.data$match)) |>
    anti_join(
      y = data_regex_match |>
        mutate(match = .extract_source_key(.data$match)),
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
  concept_keys,
  id,
  group
) {
  extract_data <- mutate(
    .data = data_match_df,
    # a span merged across a hard line break carries the raw tag. The separator
    # that admitted it always retains at least one of space, hyphen or
    # apostrophe, so dropping the tag cannot glue the two words together
    extract = .re2_extract_prepared(data_regex_prep, data_regex_str) |>
      map_chr(paste, collapse = " ; ") |>
      str_remove_all(regex("<br/>", ignore_case = TRUE))
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

  # the pivot can only name columns it finds rows for, so a concept no surviving
  # match reaches would make the block follow the corpus, not the declared set
  extract_concept_dummy[
    setdiff(concept_keys, names(extract_concept_dummy))
  ] <- 0

  extract_concept_dummy <- relocate(
    extract_concept_dummy,
    all_of(concept_keys),
    .after = last_col()
  )

  lst(
    data = extract_data,
    concept_name = extract_concept_name,
    concept_dummy = extract_concept_dummy
  ) |>
    reduce(inner_join, by = c(id, group)) |>
    relocate("concept", "extract", .after = last_col()) |>
    arrange(pick(all_of(c(group, id)))) |>
    mutate(n = row_number(), .before = 1L)
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
    id = map(
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
