.extract_chunk_default <- 15000L

.extract_chunk_n <- \(chunk, call = rlang::caller_env()) {
  if (is.null(chunk) || isFALSE(chunk)) {
    return(NULL)
  }

  if (isTRUE(chunk)) {
    return(.extract_chunk_default)
  }

  if (
    !is.numeric(chunk) ||
      length(chunk) != 1L ||
      !is.finite(chunk) ||
      chunk < 1 ||
      chunk != trunc(chunk)
  ) {
    default <- .extract_chunk_default

    cli_abort(
      c(
        "{.arg chunk} must be {.code TRUE}, {.code NULL} or a single whole number, {.val {1}} or more",
        "x" = "Got {.obj_type_friendly {chunk}} of length {length(chunk)}.",
        "i" = "It is the number of documents a chunk carries: {.code chunk = TRUE} takes the default {.val {default}}."
      ),
      call = call
    )
  }

  as.integer(chunk)
}

.extract_chunk_guard <- \(
  data,
  id,
  group,
  sample,
  seed,
  ano_hash,
  ano_hide,
  call = rlang::caller_env()
) {
  if (!is.null(data)) {
    cli_abort(
      c(
        "{.arg chunk} reads the Parquet cache {.fn edstr_clean} wrote, so it takes no {.arg data}.",
        "i" = "Drop {.arg data} to chunk the cached corpus, or drop {.arg chunk} to extract from the frame in memory."
      ),
      call = call
    )
  }

  keys <- c("id", "group")[c(is.null(id), is.null(group))]

  if (length(keys) > 0) {
    cli_abort(
      c(
        "{.arg chunk} requires {.arg {keys}}.",
        "i" = "A chunk is a range of {.arg group}, and each subprocess receives {.arg id}, {.arg group} and the text column alone, so neither key can be detected from that projection."
      ),
      call = call
    )
  }

  unsupported <- c("sample", "seed", "ano_hash", "ano_hide")[
    !map_lgl(list(sample, seed, ano_hash, ano_hide), is.null)
  ]

  if (length(unsupported) > 0) {
    cli_abort(
      c(
        "{.arg {unsupported}} cannot be combined with {.arg chunk}.",
        "i" = "{.arg sample} would draw within each chunk rather than over the corpus, and the subprocesses never see the columns {.arg ano_hash} and {.arg ano_hide} target."
      ),
      call = call
    )
  }

  invisible(NULL)
}

.extract_chunk_ident <- \(con, x) {
  as.character(DBI::dbQuoteIdentifier(con, x))
}

.extract_chunk_plan <- \(
  path,
  id,
  group,
  text_input,
  chunk,
  chunk_sort,
  concept_keys
) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  schema <- names(DBI::dbGetQuery(
    con,
    "SELECT * FROM read_parquet(?) LIMIT 0",
    list(path)
  ))

  absent <- list(
    id = id,
    group = group,
    text_input = text_input,
    chunk_sort = chunk_sort
  ) |>
    map(~ setdiff(.x, schema)) |>
    keep(~ length(.x) > 0)

  if (length(absent) > 0) {
    cli_abort(c(
      "{.arg {names(absent)}}: column not found in the Parquet cache",
      "x" = "Missing: {.val {unlist(absent, use.names = FALSE)}}.",
      "i" = "The cache holds {.val {schema}}."
    ))
  }

  clash <- .extract_reserved(schema, concept_keys, group)

  if (length(clash) > 0) {
    cli_abort(c(
      "The Parquet cache holds {length(clash)} column{?s} the output builds for itself.",
      "x" = "Reserved: {.val {clash}}.",
      "i" = "Rename {qty(length(clash))}{?it/them} upstream of {.fn edstr_clean}, or rename the colliding concept."
    ))
  }

  q_id <- .extract_chunk_ident(con, id)
  q_group <- .extract_chunk_ident(con, group)

  keys <- DBI::dbGetQuery(
    con,
    glue(
      "SELECT count(*) AS doc, count(DISTINCT {q_id}) AS id_distinct,
       count({q_id}) AS id_filled, count({q_group}) AS group_filled
       FROM read_parquet(?)"
    ),
    list(path)
  )

  if (keys$doc == 0) {
    cli_abort("The Parquet cache {.path {path}} holds no row")
  }

  if (keys$id_filled != keys$doc) {
    cli_abort(c(
      "{.arg id}: {.val {id}} contains missing values",
      "i" = "The chunk plan reads it as the unique document key."
    ))
  }

  if (keys$id_distinct != keys$doc) {
    cli_abort(c(
      "{.arg id}: {.val {id}} is not unique in the Parquet cache",
      "x" = "{keys$id_distinct} distinct value{?s} over {keys$doc} row{?s}.",
      "i" = "The chunk plan reads it as the unique document key."
    ))
  }

  if (keys$group_filled != keys$doc) {
    cli_abort(c(
      "{.arg group}: {.val {group}} contains missing values",
      "i" = "A chunk reads a range of {.val {group}}, so a missing value would leave its document out of every chunk."
    ))
  }

  groups <-
    DBI::dbGetQuery(
      con,
      glue(
        "SELECT {q_group} AS id_group, count(*) AS doc,
         min(file_row_number) AS row_min, max(file_row_number) AS row_max
         FROM read_parquet(?, file_row_number = true) GROUP BY 1 ORDER BY 1"
      ),
      list(path)
    ) |>
    as_tibble() |>
    mutate(
      doc = as.integer(.data$doc),
      chunk = as.integer(ceiling(cumsum(.data$doc) / chunk))
    )

  interleaved <- nrow(groups) > 1 &&
    any(groups$row_max[-nrow(groups)] >= groups$row_min[-1], na.rm = TRUE)

  if (interleaved) {
    cli_warn(c(
      "{.val {group}} is out of file order in the Parquet cache.",
      "i" = "Every chunk then scans the whole file instead of one range: sort the source by {.val {group}} before {.fn edstr_clean} to keep a read proportional to the chunk."
    ))
  }

  bound <-
    groups |>
    summarise(
      lo = first(.data$id_group),
      hi = last(.data$id_group),
      doc = sum(.data$doc),
      .by = "chunk"
    )

  oversized <- sum(groups$doc > chunk)

  if (oversized > 0) {
    widest <- which.max(groups$doc)

    cli_warn(c(
      "{oversized} group{?s} {?is/are} larger than the {.arg chunk} budget of {chunk} documents.",
      "x" = "The widest is {.val {groups$id_group[widest]}}, carrying {groups$doc[widest]} document{?s}.",
      "i" = "A chunk cannot split a {.val {group}}, so each forms a chunk of its own and no budget bounds its size.",
      "i" = "{.arg chunk} is a soft bound in any case: a group straddling a boundary goes whole into the later chunk."
    ))
  }

  DBI::dbWriteTable(
    con,
    "edstr_chunk_plan",
    as.data.frame(bound[c("chunk", "lo", "hi")]),
    temporary = TRUE
  )

  partition <-
    DBI::dbGetQuery(
      con,
      glue(
        "SELECT p.chunk AS chunk, count(*) AS doc
         FROM read_parquet(?) d
         JOIN edstr_chunk_plan p ON d.{q_group} BETWEEN p.lo AND p.hi
         GROUP BY 1 ORDER BY 1"
      ),
      list(path)
    ) |>
    mutate(chunk = as.integer(.data$chunk), doc = as.integer(.data$doc))

  covered <- identical(partition$chunk, bound$chunk) &&
    identical(partition$doc, bound$doc) &&
    sum(partition$doc) == keys$doc

  if (!covered) {
    cli_abort(c(
      "The chunk bounds do not partition the Parquet cache.",
      "x" = "{sum(partition$doc)} document{?s} read back over {nrow(partition)} of {nrow(bound)} chunk{?s}, against {keys$doc} row{?s} planned.",
      "i" = "The bounds are ranges of {.val {group}}; a value sorting differently in R and in the reader breaks them."
    ))
  }

  lst(
    bound = bound,
    doc_total = as.integer(keys$doc),
    cols = lst(
      schema = schema,
      select = .extract_chunk_ident(con, unique(c(id, group, text_input))),
      meta = setdiff(schema, c(group, text_input)),
      group = q_group
    )
  )
}

.extract_chunk_read <- \(path, cols, lo, hi) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "SET memory_limit = '2GB'")

  DBI::dbGetQuery(
    con,
    glue(
      "SELECT {paste(cols$select, collapse = ', ')}
       FROM read_parquet(?) WHERE {cols$group} BETWEEN ? AND ?"
    ),
    list(path, lo, hi)
  ) |>
    as_tibble()
}

.extract_chunk_meta <- \(path, cols) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbGetQuery(
    con,
    glue(
      "SELECT {paste(.extract_chunk_ident(con, cols$meta), collapse = ', ')}
       FROM read_parquet(?)"
    ),
    list(path)
  ) |>
    as_tibble()
}

.extract_chunk_local <- \(job, config, path, cols, args) {
  local_options(list(
    edstr_dirname = config$dirname,
    edstr_filename = config$filename,
    edstr_text = config$text_input,
    edstr_overwrite = TRUE,
    edstr_profile = config$profile
  ))

  data <- .extract_chunk_read(path, cols, job$lo, job$hi)

  do.call(
    edstr_extract,
    c(
      list(data = data),
      args,
      list(
        dirname_suffix = job$dirname_suffix,
        filename_suffix = job$filename_suffix
      )
    )
  )

  invisible(NULL)
}

.extract_chunk_spawn <- \(job, config, path, cols, args) {
  if (exists(".__DEVTOOLS__", envir = asNamespace("edstr"), inherits = FALSE)) {
    cli_warn(c(
      "The chunk subprocesses load the installed {.pkg edstr}, not the one under development.",
      "i" = "Install the package before reading a chunked run as a test of the source tree."
    ))
  }

  callr::r(
    func = \(job, config, path, cols, args) {
      utils::getFromNamespace(".extract_chunk_local", "edstr")(
        job,
        config,
        path,
        cols,
        args
      )
    },
    args = list(
      job = job,
      config = config,
      path = path,
      cols = cols,
      args = args
    ),
    show = TRUE
  )
}

.extract_chunk_bind <- \(chunks, pick) {
  map(chunks, pick) |> list_rbind()
}

.extract_chunk_regroup <- \(x, values, sort = TRUE) {
  keys <- setdiff(names(x), values)

  out <- summarise(x, across(all_of(values), sum), .by = all_of(keys))

  if (sort) {
    out <- arrange(out, desc(.data[[values[[1]]]]), pick(all_of(keys)))
  }

  select(out, all_of(names(x)))
}

.extract_chunk_combine <- \(
  chunks,
  meta,
  cols,
  concepts_list,
  params,
  args,
  chunk_sort
) {
  id <- args$id
  group <- args$group
  text_input <- args$text_input

  values <- c("match", id, group)

  data_match <- .extract_chunk_bind(chunks, ~ .x$match$init)
  data_id <- .extract_chunk_bind(chunks, ~ .x$match$final$keep)

  data_match_final <- lst(
    keep = data_id,
    drop = .extract_chunk_bind(chunks, ~ .x$match$final$drop)
  )

  data_match_exclus <- map(
    set_names(c("auto", "manual")),
    \(mode) .extract_chunk_bind(chunks, ~ .x$exclus$match[[mode]])
  )

  data_count <- .extract_chunk_regroup(
    .extract_chunk_bind(chunks, ~ .x$count$final),
    values
  )

  data_token_match <- map(
    set_names(names(chunks[[1]]$count$init)),
    \(size) {
      .extract_chunk_regroup(
        .extract_chunk_bind(chunks, ~ .x$count$init[[size]]),
        "match"
      )
    }
  )

  data_count_exclus <- .extract_chunk_regroup(
    .extract_chunk_bind(chunks, ~ .x$exclus$count),
    values,
    sort = FALSE
  )

  data_unmatched <- lst(
    unmatched = lst(
      n_no_concept = sum(map_int(chunks, ~ .x$unmatched$n_no_concept)),
      no_concept = .extract_chunk_bind(chunks, ~ .x$unmatched$no_concept),
      no_source = .extract_chunk_bind(chunks, ~ .x$unmatched$no_source),
      empty_text = .extract_chunk_bind(chunks, ~ .x$unmatched$empty_text),
      outside_p = .extract_chunk_bind(chunks, ~ .x$unmatched$outside_p)
    ),
    mismatched = .extract_chunk_bind(chunks, ~ .x$mismatched)
  )

  data_regex_match <-
    .extract_chunk_bind(chunks, ~ .x$regex$match) |>
    arrange(.data$concept)

  data_regex_count <- count(
    x = data_regex_match,
    .data$concept,
    .data$match,
    sort = TRUE
  )

  source_regex <- .extract_source_regex(
    data_count,
    text_input,
    args$regex_replace
  )

  decorate <- \(x, keep) {
    x |>
      left_join(meta, by = id, relationship = "one-to-one") |>
      select(all_of(keep))
  }

  order_by <- c(group, chunk_sort, id)

  decorate_rows <- \(x, keep) {
    x |>
      decorate(keep) |>
      arrange(pick(all_of(order_by))) |>
      mutate(n = row_number())
  }

  base_cols <- setdiff(cols$schema, text_input)

  data_extract <- decorate_rows(
    .extract_chunk_bind(chunks, ~ .x$data$extract),
    c("n", cols$schema, concepts_list$keys, "concept", "extract")
  )

  data_note <- decorate_rows(
    .extract_chunk_bind(chunks, ~ .x$data$note),
    c("n", cols$schema, "concept", "extract")
  )

  data_summary <- .extract_summary(
    data_match,
    data_match_exclus,
    data_id,
    data_count,
    id,
    group,
    params
  )

  data_sheets <- .extract_sheets(
    data_extract,
    data_id,
    data_count,
    data_count_exclus,
    data_summary,
    data_unmatched,
    source_regex$data_regex_df,
    data_regex_match,
    data_regex_count,
    source_regex$regex_replace_df,
    concepts_list,
    text_input
  )

  data_sheets_gt <- if (isTRUE(args$save_as_gt)) {
    check_installed("gt")

    .extract_sheets_gt(
      data_sheets,
      concepts_list,
      id,
      text_input,
      args$concept_color,
      args$text_color
    )
  } else {
    NULL
  }

  lst(
    data_save = lst(
      data = lst(
        base = decorate(
          .extract_chunk_bind(chunks, ~ .x$data$base),
          base_cols
        ),
        match = decorate(
          .extract_chunk_bind(chunks, ~ .x$data$match),
          base_cols
        ),
        extract = data_extract,
        note = data_note
      ),
      regex = lst(
        concepts = concepts_list$regex_df,
        replace = source_regex$regex_replace_df,
        final = source_regex$data_regex_df,
        match = data_regex_match
      ),
      match = lst(init = data_match, final = data_match_final),
      count = lst(init = data_token_match, final = data_count),
      exclus = lst(match = data_match_exclus, count = data_count_exclus),
      unmatched = data_unmatched$unmatched,
      mismatched = data_unmatched$mismatched,
      summary = data_summary,
      sheets = lst(
        df = map(data_sheets, ~ if (is.data.frame(.x)) .x else .x$data),
        gt = data_sheets_gt
      )
    ),
    sheets = data_sheets
  )
}

.extract_chunk_check <- \(chunks, files, params, doc_total, id) {
  stale <- !map_lgl(chunks, ~ identical(.x$summary$params, params))

  if (any(stale)) {
    cli_abort(c(
      "{sum(stale)} chunk file{?s} {?was/were} run with other arguments.",
      "x" = "Stale: {.path {files[stale]}}.",
      "i" = "Delete {qty(sum(stale))}{?it/them}, or set {.code edstr_overwrite = TRUE} in {.fn edstr_config} to run every chunk again."
    ))
  }

  doc_read <- sum(map_int(chunks, ~ nrow(.x$data$base)))
  ids <- map(chunks, ~ .x$data$base[[id]]) |> unlist(use.names = FALSE)

  if (doc_read != doc_total || anyDuplicated(ids) != 0L) {
    cli_abort(c(
      "The chunk files do not partition the corpus.",
      "x" = "{doc_read} document{?s} over {length(unique(ids))} distinct {.val {id}}, against {doc_total} row{?s} planned.",
      "i" = "Delete the chunk directories and run the extraction again."
    ))
  }

  invisible(chunks)
}

.extract_chunk_clear <- \(paths, files) {
  dirs <- map_chr(paths, ~ as.character(.x$save_dir))

  gone <- map2_lgl(dirs, files, \(dir, file) {
    if (!fs::dir_exists(dir) || !fs::file_exists(file)) {
      return(FALSE)
    }

    fs::dir_delete(dir)

    TRUE
  })

  if (any(gone)) {
    cli_alert_info("{sum(gone)} chunk director{?y/ies} removed")
  }

  invisible(gone)
}

.extract_chunked <- \(
  args,
  chunk,
  chunk_sort,
  dirname_suffix,
  filename_suffix,
  save_dir,
  save_files,
  save_extract
) {
  check_installed(
    c("callr", "DBI", "duckdb"),
    reason = "to run a chunked extraction."
  )

  path <- fs::path_abs(check_config("clean")$save)

  if (!fs::file_exists(path)) {
    cli_abort(c(
      "The Parquet cache {.path {path}} is not found",
      "i" = "{.arg chunk} reads what {.fn edstr_clean} saved; run it first, or pass {.arg data} and drop {.arg chunk}."
    ))
  }

  if (is.null(args$concepts)) {
    cli_abort(c(
      "{.arg concepts} is not set",
      "i" = "Pass a named pattern, e.g. {.code concepts = c(diabete = \"diabet\")}"
    ))
  }

  concepts_list <- .extract_parse_concepts(
    args$concepts,
    args$collapse,
    args$intersect,
    args$starts_with_only
  )

  tic.clearlog()
  tic("Full steps")

  cli_h1("edstr_extract")
  br()

  plan <- .timed(
    "Chunk plan",
    .extract_chunk_plan(
      path = path,
      id = args$id,
      group = args$group,
      text_input = args$text_input,
      chunk = chunk,
      chunk_sort = chunk_sort,
      concept_keys = concepts_list$keys
    )
  )

  cli_alert_info(
    "{.strong {nrow(plan$bound)} chunk{?s}} of about {chunk} document{?s} over {plan$doc_total} {args$id}"
  )
  br()

  jobs <- map(seq_len(nrow(plan$bound)), \(i) {
    suffix <- paste0("chunk_", plan$bound$chunk[[i]])

    c(
      as.list(plan$bound[i, ]),
      list(
        dirname_suffix = paste(c(dirname_suffix, suffix), collapse = "_"),
        filename_suffix = paste(c(filename_suffix, suffix), collapse = "_")
      )
    )
  })

  paths <- map(jobs, ~ .extract_paths(.x$dirname_suffix, .x$filename_suffix))

  files <- map_chr(paths, ~ as.character(.x$save_extract$rds))

  config <- list(
    dirname = fs::path_abs(getOption("edstr_dirname")),
    filename = getOption("edstr_filename"),
    text_input = args$text_input,
    profile = getOption("edstr_profile")
  )

  pending <- if (isTRUE(getOption("edstr_overwrite"))) {
    seq_along(jobs)
  } else {
    which(!fs::file_exists(files))
  }

  walk(pending, \(i) {
    cli_progress_step(
      "{.strong Chunk {plan$bound$chunk[[i]]} of {nrow(plan$bound)}}"
    )
    br()

    rlang::try_fetch(
      .extract_chunk_spawn(jobs[[i]], config, path, plan$cols, args),
      error = \(cnd) {
        cli_abort(
          c(
            "Chunk {plan$bound$chunk[[i]]} of {nrow(plan$bound)} failed.",
            "i" = "It covers {.val {jobs[[i]]$lo}} to {.val {jobs[[i]]$hi}}, {jobs[[i]]$doc} document{?s}. The chunks already written are kept, so the run resumes from this one."
          ),
          parent = cnd
        )
      }
    )
  })

  missing <- !fs::file_exists(files)

  if (any(missing)) {
    cli_abort(c(
      "{sum(missing)} chunk{?s} produced no output.",
      "x" = "Missing: {.path {files[missing]}}."
    ))
  }

  cli_progress_step("{.strong Combining {length(files)} chunk{?s}}")
  br()

  params <- list(
    sample = NULL,
    seed = NULL,
    id = args$id,
    group = args$group,
    ngram_max = args$ngram_max,
    concepts_root = concepts_list$root,
    concepts_names = concepts_list$regex_df$concept_name,
    collapse = args$collapse,
    intersect = args$intersect,
    starts_with_only = args$starts_with_only,
    exclus_manual = args$exclus_manual,
    exclus_auto_escape = args$exclus_auto_escape,
    exclus_auto_token_min = args$exclus_auto_token_min,
    regex_replace = args$regex_replace,
    unmatched_data = args$unmatched_data,
    concept_color = args$concept_color,
    text_color = args$text_color,
    save_as_gt = args$save_as_gt
  )

  chunks <- .extract_chunk_check(
    .timed("Read chunks", map(files, readRDS)),
    files,
    params,
    plan$doc_total,
    args$id
  )

  meta <- .timed("Read metadata", .extract_chunk_meta(path, plan$cols))

  combined <- .timed(
    "Combine",
    .extract_chunk_combine(
      chunks = chunks,
      meta = meta,
      cols = plan$cols,
      concepts_list = concepts_list,
      params = params,
      args = args,
      chunk_sort = chunk_sort
    )
  )

  data_save <- combined$data_save

  rm(chunks)
  invisible(gc(full = TRUE))

  fs::dir_create(save_dir, mode = "0700")

  cli_progress_step("{.strong Saving file {save_files}}")
  br()

  wb <- .timed(
    "Build xlsx",
    .extract_sheets_xlsx(
      combined$sheets,
      data_save$match$final$keep,
      concepts_list,
      args$text_input,
      args$concept_color,
      args$text_color
    )
  )

  .timed("Save xlsx", wb_save(wb = wb, file = save_extract$xlsx))

  .timed(
    "Save json",
    jsonlite::write_json(
      x = data_save$summary,
      path = save_extract$json,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  )

  .timed("Save rds", saveRDS(data_save, file = save_extract$rds))

  cli_progress_done()

  .extract_chunk_clear(paths, files)

  br()
  cli_rule()
  br()

  cli_h2("Timing per step")
  cli_verbatim(unlist(tic.log(format = TRUE)))
  br()

  toc()
  br()

  cli_alert_success(
    "{.strong {nrow(data_save$data$extract)} {args$id}} retained over {plan$doc_total}, combined from {length(files)} chunk{?s}"
  )
  cli_ul("{nrow(data_save$count$final)} distinct match{?es}")
  cli_ul("{data_save$unmatched$n_no_concept} unmatched (no concept)")

  br()
  cli_alert_success(
    "{.strong Files saved in {cli::col_blue(fs::path(save_dir))}}"
  )
  br()
  cli_rule()

  data_save
}
