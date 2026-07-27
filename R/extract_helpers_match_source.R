# equality key between a token and a source span: the two differ by accents,
# case and whichever separator the source happened to carry. Both the repair
# pass and the `mismatched` anti-join rest on it, and they must agree exactly,
# a drift of one substitution moving the mismatch count with nothing to show it
.extract_source_key <- \(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    tolower() |>
    # the separators source matching accepts for a token space, per the
    # `[\s\-']+` substitution in `.extract_match_source()`
    str_replace_all(c("-(<br/>)?|-?<br/>" = " ", "'" = " ", "\\s+" = " "))
}

# a token the alternation failed to confirm can still be present on its own: the
# branch that won the position displaced it, and every n-gram has to be tagged.
# Re-testing each one alone recovers the span, and the work is proportional to
# the failures rather than to the corpus, which is what keeps this affordable.
# It has to go through the offset machinery, not a boolean detect: the row it
# adds carries the original surface form, accents and case included.
.extract_repair_source <- \(
  data_id,
  data_regex_match,
  prep,
  prep_ids,
  regex_replace,
  regex_wrap,
  id,
  text_input
) {
  unconfirmed <-
    data_id |>
    select(all_of(id), "concept", token = all_of(text_input)) |>
    mutate(key = .extract_source_key(.data$token)) |>
    anti_join(
      y = data_regex_match |>
        mutate(key = .extract_source_key(.data$match)) |>
        select(all_of(id), "concept", "key"),
      by = c(id, "concept", "key")
    ) |>
    distinct(pick(all_of(c(id, "concept"))), .data$token)

  if (nrow(unconfirmed) == 0) {
    return(NULL)
  }

  grouped <- nest(unconfirmed, rows = all_of(id), .by = c("concept", "token"))

  hits <- map2(grouped$token, grouped$rows, \(token, rows) {
    pattern <- as.character(glue(
      regex_wrap,
      x = str_replace_all(token, regex_replace)
    ))

    found <- .re2_extract_prepared(
      .re2_prep_subset(prep, match(rows[[id]], prep_ids)),
      pattern
    )

    tibble(
      !!id := rep(rows[[id]], lengths(found)),
      match = unlist(found, use.names = FALSE)
    )
  })

  found <-
    grouped |>
    mutate(hit = hits) |>
    select("concept", "hit") |>
    unnest("hit") |>
    select(all_of(id), "concept", "match")

  # the key above compares a token to a span, so it cannot see a `regex_replace`
  # widening the user supplied: a token confirmed only through one reads as
  # unconfirmed and its span is found a second time here. Deciding on the span
  # itself, and on how many times it already stands, is what keeps the pass
  # additive; the key stays as the cheap prefilter it is good for
  by <- c(id, "concept", "match")

  add <-
    found |>
    count(across(all_of(by)), name = "found") |>
    left_join(
      count(data_regex_match, across(all_of(by)), name = "have"),
      by = by
    ) |>
    mutate(add = .data$found - ifelse(is.na(.data$have), 0L, .data$have)) |>
    filter(.data$add > 0)

  add[rep(seq_len(nrow(add)), add$add), by]
}

.extract_match_source <- \(
  data_match_df,
  data_count,
  data_id,
  text_input,
  id,
  regex_replace
) {
  regex_replace_arg <- regex_replace

  regex_replace <- c(
    "a" = "[a\u00e0\u00e2\u00e6]",
    "e" = "[e\u00e9\u00e8\u00ea\u00eb\u0153]",
    "i" = "[i\u00ee\u00ef]",
    "o" = "[o\u00f4]",
    "u" = "[u\u00f9\u00fb\u00fc]",
    "\\s" = "(?:<br/>)?[\\\\s\\\\-']+(?:<br/>)?"
  ) |>
    append(regex_replace) |>
    regex(multiline = TRUE)

  regex_wrap <- "(?i)\\b({x})\\b"

  regex_replace_df <- tibble(
    pattern = names(as.list(regex_replace)),
    replace = regex_replace
  )

  # branch order decides which alternative wins a position on the ICU paths,
  # which have no longest-match mode: `set_class_css()` and the fallback of
  # `.re2_extract_prepared()`. A shorter alternative placed first consumes the
  # position, so the longer expression arrives split into two spans and its tail
  # stays unhighlighted. Sorting by token length only approximates that fix, an
  # expanded branch not matching a span as long as the token that produced it;
  # re2 takes the exact mode instead. Ties keep `count()`'s descending
  # frequency, `arrange()` being stable.
  data_regex_replace <-
    data_count |>
    arrange(desc(nchar(.data[[text_input]]))) |>
    mutate(!!text_input := str_replace_all(.data[[text_input]], regex_replace))

  data_regex_str <- glue(
    regex_wrap,
    x = data_regex_replace |> pull(text_input) |> paste(collapse = "|")
  )

  data_regex_df <-
    data_regex_replace |>
    arrange(.data$concept) |>
    nest(!!text_input := all_of(text_input), .by = "concept") |>
    mutate(
      !!text_input := map_chr(
        .data[[text_input]],
        ~ str_flatten(unlist(.), "|")
      ),
      !!text_input := glue(regex_wrap, x = .data[[text_input]])
    )

  data_regex_list <-
    data_regex_df[[text_input]] |>
    as.list() |>
    set_names(data_regex_df$concept)

  data_regex_prep <- .timed_sub(
    "  source: re2 prepare",
    .re2_prepare(data_match_df[[text_input]])
  )

  data_regex_match <- .timed_sub(
    "  source: K passes",
    imap(
      data_regex_list,
      ~ view_output(
        data = data_match_df,
        text_input = text_input,
        pattern = .x,
        id = id,
        error_empty = FALSE,
        match_only = TRUE,
        prep = data_regex_prep
      ) |>
        pluck("match") |>
        mutate(concept = .y, .before = "match")
    ) |>
      list_rbind()
  )

  if (ncol(data_regex_match) == 0) {
    data_regex_match <- tibble(
      !!id := data_match_df[[id]][0],
      concept = character(),
      match = character()
    )
  }

  # stable, so the K-pass rows keep their order and a repaired one lands in its
  # own concept block rather than in a tail of a different provenance
  data_regex_match <- .timed_sub(
    "  source: repair",
    bind_rows(
      data_regex_match,
      .extract_repair_source(
        data_id,
        data_regex_match,
        data_regex_prep,
        data_match_df[[id]],
        regex_replace,
        regex_wrap,
        id,
        text_input
      )
    ) |>
      arrange(.data$concept)
  )

  data_regex_count <- .timed_sub(
    "  source: count",
    count(
      x = data_regex_match,
      .data$concept,
      .data$match,
      sort = TRUE
    )
  )

  list(
    regex_replace_arg = regex_replace_arg,
    regex_replace_df = regex_replace_df,
    data_regex_df = data_regex_df,
    data_regex_list = data_regex_list,
    data_regex_match = data_regex_match,
    data_regex_count = data_regex_count,
    data_regex_str = data_regex_str,
    data_regex_prep = data_regex_prep
  )
}
