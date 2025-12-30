#' Title
#'
#' @param data data
#' @param text_input text_input
#' @param sample sample
#' @param seed seed
#' @param ano_hash ano_hash
#' @param ano_hide ano_hide
#' @param id id
#' @param group group
#' @param token token
#' @param concepts concepts
#' @param collapse collapse
#' @param intersect intersect
#' @param starts_with_only starts_with_only
#' @param exclus_manual exclus_manual
#' @param exclus_auto_escape exclus_auto_escape
#' @param regex_replace regex_replace
#' @param mismatch_data mismatch_data
#' @param concept_color concept_color
#' @param text_color text_color
#' @param text_background text_background
#' @param dirname_suffix dirname_suffix
#' @param filename_suffix filename_suffix
#' @param load load
#'
#' @return value
#' @export
#'
#' @examples example
#'
edstr_extract <- \(
  data,
  text_input = getOption("edstr_text"),
  id = check_id_key(data = data, exclude = text_input),
  group = NULL,
  sample = NULL,
  seed = NULL,
  ano_hash = NULL,
  ano_hide = NULL,
  token = 1,
  concepts,
  collapse = FALSE,
  intersect = FALSE,
  starts_with_only = TRUE,
  exclus_manual = NULL,
  exclus_auto_escape = NULL,
  regex_replace = NULL,
  mismatch_data = FALSE,
  concept_color = "#0099FF",
  text_color = "#FF0000",
  text_background = "#FFFF00",
  dirname_suffix = if (!is.null(sample)) glue("sample_{sample}") else NULL,
  filename_suffix = dirname_suffix,
  load = FALSE
) {

  check_class(data, "data.frame")
  check_class(text_input, "character")

  config <- check_config()

  config_dir <- getOption('edstr_dirname')
  filename <- getOption('edstr_filename')
  dirname <- "extract"

  save_dir <- glue("{config_dir}/{dirname}")

  if (!is.null(dirname_suffix)) save_dir <- glue("{save_dir}_{dirname_suffix}")

  save_files <- glue("{filename}_{dirname}")

  if (!is.null(filename_suffix)) save_files <- glue("{save_files}_{filename_suffix}")

  if (!file.exists(save_dir) && !load) dir.create(path = save_dir)

  save_extract <- glue("{save_dir}/{save_files}")

  save_extract_rds <- glue("{save_extract}.rds")

  if (!is.null(seed)) set.seed(seed)

  tic("Full steps")

  cli_h1("edstr_extract")

  if (!load) {

### CONCEPTS -------------------------------------------------------------------

  if (length(concepts) > 1) {

    if (!is_named(concepts)) {

      cli_abort("Chaque concept doit \u00eatre nomm\u00e9")

    } else {

      concepts <- as.list(concepts)

    }

  } else if (!is.list(concepts)) {

    if (!is_named(concepts)) {

      concepts <- list("<concept>" = concepts)

    } else {

      concepts <- as.list(concepts)

    }

  }

  list_flatten_complete <- \(data) {

    data |>
      pluck_depth() |>
      seq() |>
      reduce(~ list_flatten(.), .init = data)

  }

  if (collapse) {

    if (length(concepts) == 1) {

      cli_abort("Pas de regroupement possible si un seul concept est recherch\u00e9")

    }

    if (pluck_depth(concepts) > 2) {

      concepts <-
      concepts |>
        map(~ . |>
              list_flatten_complete() |>
              paste(collapse = "|"))

    } else {

      concepts <-
      concepts |>
        paste(collapse = "|") |>
        set_names("<concept>")

    }

  } else {

    concepts <- list_flatten_complete(concepts)

  }

  concepts_keys <- names(concepts)

  concepts_names <- concepts |> imap(~ { if (is_named(.x)) names(.x) else .y })

  concepts_root <- concepts_keys |> str_remove("_.+") |> unique()

  if (length(concepts_root) == 1 && intersect) {

    cli_abort("Pas d'intersection possible si un seul concept est recherch\u00e9")

  }

  str_concepts_comma <- str_flatten_comma(concepts_root)
  str_concepts_inter <- glue("[{paste(concepts_root, collapse = ' ET ')}]")

  .regex_end <- if (starts_with_only) "\\S*$" else ""

  concepts_regex <- map(concepts, ~ glue("^({.}){.regex_end}"))

  concepts_regex_df <- tibble(
    concept_key = concepts_keys,
    concept_name = unlist(concepts_names),
    regex = unlist(concepts_regex)
  )

### ID -------------------------------------------------------------------------

  which_key <- check_id_key(
    data = data,
    exclude = text_input,
    error = FALSE
  )

  if (!(id %in% which_key)) rlang::arg_match(id, which_key)

  which_group <- check_id_group(
    data = data,
    id = group
  )

  if (is.null(group)) {

    group <- "id_group"

    data <- mutate(
      .data = data,
      !!group := row_number(),
      .after = all_of(id)
    )

  }

  nrow_init <- nrow(data)

  if (!is.null(sample)) data <- data[sample(nrow_init, sample), ]

### FORMAT ---------------------------------------------------------------------

  cli_progress_step("{.strong Formatage du texte source}")
  cli_text("\n\n")

  if (!is.null(ano_hash) || !is.null(ano_hide)) {

    data <- easy_ano(
      x = data,
      to_hash = ano_hash,
      to_hide = ano_hide
    )

  }

  data_token <-
  data[c(id, group, text_input)] |>
    mutate(!!text_input := easy_format(.data[[text_input]]))

### TOKENISATION ---------------------------------------------------------------

  cli_progress_step("{.strong Tokenisation du texte source}")
  cli_text("\n\n")

  token <- token |> set_names(paste0("n", token))

  data_token <- map(
    token,
    ~ easy_ngram(
      data = data_token,
      text = !!text_input,
      n = .,
      filter = "[:alpha:]"
    )
  )

### MATCHING TOKEN -------------------------------------------------------------

  cli_progress_step("{.strong Matching du texte tokenis\u00e9}")
  cli_text("\n\n")

  token_extract <- \(x, n) {

    x |>
      imap(~ data_token[[n]] |>
             mutate(!!text_input := str_extract(.data[[text_input]], .x),
                    concept = .y,
                    .before = text_input) |>
             drop_na())

  }

  data_token_list <- map(
    token,
    ~ concepts_regex |>
      token_extract(.) |>
      list_rbind()
  )

  data_token_match <- map(
    data_token_list,
    ~ count(
      x = .,
      concept, pick(text_input),
      name = "match",
      sort = TRUE
    )
  )

  data_match_init <-
  imap(data_token_list, ~ mutate(.x, token = .y)) |>
    list_rbind()

  .concepts_id <-
  set_names(concepts_root) |>
    map(
      ~ data_match_init |>
        distinct(pick(id, group), concept) |>
        pivot_wider(names_from = concept,
                    values_from = concept) |>
        filter(if_any(matches(.), ~ !is.na(.))) |>
        select(id, group)
    )

  if (intersect) {

    .match_id <- .concepts_id |> reduce(inner_join, by = c(id, group))

  } else {

    .match_id <- data_match_init[id]

  }

  data_match <-
  data_match_init |>
    rename(concept_key = concept) |>
    mutate(concept = concept_key |> map_chr(~ concepts_names[[.]]),
           .after = concept_key) |>
    filter(.data[[id]] %in% .match_id[[id]])

  data_match_init_df <-
  data |>
    select(-text_input) |>
    filter(.data[[id]] %in% data_match_init[[id]])

  data_match_df <- data |> filter(.data[[id]] %in% data_match[[id]])

  rm(data_token); invisible(gc())

  if (nrow(data_match) == 0) {

    abort_intersect <- if (intersect) " \u00e0 l'intersection" else ""

    cli_abort("{.strong Aucune correspondance{abort_intersect}}")

  }

### EXCLUSIONS -----------------------------------------------------------------

  cli_progress_step("{.strong Exclusions}")
  cli_text("\n\n")

  if (!is.null(exclus_auto_escape)) {

    data_match <- filter(data_match, !str_detect(.data[[text_input]], exclus_auto_escape))

  }

  set_exclus_auto <- \(regex, name) {

    data_match[[text_input]] |>
      unique() |>
      map(~ data_match |>
            filter(str_detect(.data[[text_input]], glue(regex))) |>
            mutate(!!name := .)) |>
      list_rbind()

  }

  str_detect_safe <- \(string, pattern) {

    if (is.null(pattern) || is.na(pattern)) {

      rep(FALSE, length(string))

    } else {

      str_detect(string, pattern)

    }

  }

  data_match_exclus <-
  list(
    auto =
      list(start = "^{.}\\s",
           end = "\\s{.}$",
           start_end = "{.}.+{.}$") |>
        imap(set_exclus_auto) |>
        reduce(full_join, by = names(data_match)) |>
        filter(token > 2),
    manual =
      data_match |>
        filter(str_detect_safe(.data[[text_input]], exclus_manual))
  ) |>
    imap(~ .x |> mutate(mode = .y, .before = everything()))

  .match_exclus <-
  list(concept =
         data_match_exclus |>
           list_rbind() |>
           pull(text_input) |>
           unique(),
       expr = expr(.data[[text_input]] %in% .match_exclus$concept))

  data_match_final <-
  list(keep = data_match |> filter(!eval(.match_exclus$expr)),
       drop = data_match |> filter(eval(.match_exclus$expr)))

  data_count <-
  data_match_final |>
    map(~ list2(match = .,
                !!id := distinct(., pick(-group)),
                !!group := distinct(., pick(-id))) |>
          imap(~ . |>
                 count(token, concept, pick(text_input),
                       name = .y,
                       sort = TRUE)) |>
          reduce(left_join, by = c("concept", text_input, "token")))

  data_count_exclus <-
  data_match_exclus |>
    map(~ . |> select(-id, -group)) |>
    list_rbind() |>
    distinct() |>
    relocate(token, .before = concept) |>
    left_join(data_count$drop,
              by = c("token", "concept", text_input))

  data_id <- data_match_final$keep
  data_count <- data_count$keep

### MATCHING SOURCE ------------------------------------------------------------

  cli_progress_step("{.strong Matching du texte source}")
  cli_text("\n\n")

  regex_replace_arg <- regex_replace

  regex_replace <-
  c("e(?!$)" = "[e\u00e9\u00e8\u00ea\u00eb]",
    "(?<=o)i" = "[i\u00ee\u00ef]",
    "\\s" = "(?:<br/>)?[\\\\s\\\\-']+(?:<br/>)?") |>
    append(regex_replace) |>
    regex(multiline = TRUE)

  regex_wrap <- "(?i)\\b({x})\\b"

  regex_replace_df <-
  tibble(pattern = names(as.list(regex_replace)),
         replace = regex_replace)

  data_regex_replace <-
  data_count |>
    arrange(concept) |>
    mutate(!!text_input :=
             str_replace_all(.data[[text_input]], regex_replace))

  data_regex_str <-
  glue(regex_wrap,
       x =
         data_regex_replace |>
           pull(text_input) |>
           paste(collapse = "|"))

  data_regex_df <-
  data_regex_replace |>
    nest(!!text_input := all_of(text_input), .by = concept) |>
    mutate(!!text_input :=
             map_chr(.data[[text_input]], ~ str_flatten(unlist(.), "|")),
           !!text_input :=
             glue(regex_wrap, x = .data[[text_input]]))

  data_regex_list <-
  data_regex_df[[text_input]] |>
    as.list() |>
    set_names(data_regex_df$concept)

  data_regex_match <-
  data_regex_list |>
    imap(
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

### MISMATCH -------------------------------------------------------------------

  cli_progress_step("{.strong Mismatch entre texte source et tokenis\u00e9}")
  cli_text("\n\n")

  data_id_mismatch_base <- data[c(id, group)]

  if (mismatch_data) {

    data_id_mismatch <-
    data_id_mismatch_base |>
      filter(!.data[[id]] %in% data_match_init[[id]])

  } else {

    data_id_mismatch <-
    data_id_mismatch_base |>
      filter(.data[[id]] %in% NA_character_)

  }

  data_regex_match_conv <-
  data_regex_match |>
    mutate(match =
             match |>
               iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
               tolower() |>
               str_replace_all(c("-(<br/>)?|-?<br/>" = " ",
                                 "\\s+" = " ")))

  data_mismatch <-
  list(id = data_id_mismatch,
       regex =
         data_match |>
           select(id, concept, match = !!text_input) |>
           anti_join(y = data_regex_match_conv,
                     by = c(id, "concept", "match")))

### EXTRACTION -----------------------------------------------------------------

  cli_progress_step("{.strong Extraction}")
  cli_text("\n\n")

  data_extract <-
  list(data =
         data_match_df |>
           mutate(extract =
                    .data[[text_input]] |>
                      str_extract_all(data_regex_str) |>
                      map_chr(paste, collapse = " ; ")),
       concept_name =
         data_regex_list |>
           imap(~ data_match_df |>
                  mutate(extract = str_extract(.data[[text_input]], .x),
                         concept = .y) |>
                  drop_na(extract) |>
                  select(id, group, concept)) |>
           list_rbind() |>
           nest(concept = concept) |>
           mutate(concept = map_chr(concept, ~ str_flatten(unlist(.), " ; "))),
       concept_dummy =
         data_id |>
           distinct(pick(id, group), concept_key) |>
           pivot_wider(names_from = concept_key,
                       values_from = concept_key) |>
           mutate(across(matches(concepts_root), ~ ifelse(is.na(.), 0, 1)))) |>
    reduce(inner_join, by = c(id, group)) |>
    relocate(concept, extract, .after = last_col()) |>
    arrange(pick(group, id)) |>
    rownames_to_column("n")

### RESUME ---------------------------------------------------------------------

  cli_progress_step("{.strong R\u00e9sum\u00e9}")
  cli_text("\n\n")

  set_summary <- \(var) {

    list(total = data_match,
         exclus_auto = data_match_exclus$auto,
         exclus_manual = data_match_exclus$manual,
         final = data_id,
         distinct = data_count) |>
      set_names(~ glue("match_{.}")) |>
      imap(~ .x |>
             count(pick(all_of(var)),
                   sort = TRUE,
                   name = .y))

  }

  data_summary <-
  list(token = set_summary("token"),
       concept =
         list(match = set_summary("concept"),
              id =
                list(id, group) |>
                  imap(~ summarise(data_id,
                                   !!. := n_distinct(.data[[.]]),
                                   .by = concept))) |>
           list_flatten()) |>
    imap(~ reduce(., left_join, by = .y)) |>
    append(list(params =
                  list(sample = sample,
                       seed = seed,
                       id = id,
                       group = group,
                       token = token,
                       concepts_root = concepts_root,
                       concepts_names = concepts_regex_df$concept_name,
                       collapse = collapse,
                       intersect = intersect,
                       starts_with_only = starts_with_only,
                       exclus_manual = exclus_manual,
                       exclus_auto_escape = exclus_auto_escape,
                       regex_replace = regex_replace_arg,
                       mismatch_data = mismatch_data,
                       concept_color = concept_color,
                       text_color = text_color,
                       text_background = text_background)))

  unlink(file.path(save_dir, list.files(save_dir)), recursive = TRUE)

### XLSX DATA ------------------------------------------------------------------

  cli_progress_step("{.strong Enregistrement du .xlsx}")
  cli_text("\n\n")

  .sheet_mismatch <-
  lst(
    data = data_mismatch$regex,
    rows = if (nrow(data) > 0) data else add_row(data),
    visible = if (nrow(data) > 0) "true" else "false"
  )

  .sheet_params <-
  data_summary$params |>
    map(as.character) |>
    enframe(name = "arg") |>
    mutate(
      value = map_chr(value, ~ str_flatten(unlist(.), " ; ")),
      value = ifelse(value == "", "NULL", value)
    )

  data_sheets <-
  list(
    data = data_extract |> select(-text_input),
    token_regex = concepts_regex_df,
    token_match = data_id |> select(-concept_key),
    token_count = data_count |> select(-token),
    concept_count = data_summary$concept,
    text_replace = regex_replace_df,
    text_regex = data_regex_df,
    text_match = data_regex_match,
    text_count = data_regex_count,
    mismatch = .sheet_mismatch$data,
    params = .sheet_params
  ) |>
    imap(~ list2(name = .y, data = .x))

  .wb_text_var <- text_input
  .wb_concept_var <- "concept"

  xlsx_output <-
  wb_workbook() |>
    wb_add_custom(
      sheet = data_sheets$data$name,
      data = data_sheets$data$data,
      concept_var = c(data_id$concept_key, "concept"),
      concept_color = concept_color,
      text_var = "extract",
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$token_regex$name,
      data = data_sheets$token_regex$data,
      concept_var = names(concepts_regex_df) |> str_subset("concept"),
      concept_color = concept_color,
      text_var = "regex",
      text_color = text_color,
      halign = "left"
    ) |>
    wb_add_custom(
      sheet = data_sheets$token_match$name,
      data = data_sheets$token_match$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = .wb_text_var,
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$token_count$name,
      data = data_sheets$token_count$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = .wb_text_var,
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$concept_count$name,
      data = data_sheets$concept_count$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_replace$name,
      data = data_sheets$text_replace$data,
      halign = "left"
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_regex$name,
      data = data_sheets$text_regex$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = .wb_text_var,
      text_color = text_color,
      halign = "left",
      max_width = 150
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_match$name,
      data = data_sheets$text_match$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = "match",
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$text_count$name,
      data = data_sheets$text_count$data,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = "match",
      text_color = text_color
    ) |>
    wb_add_custom(
      sheet = data_sheets$mismatch$name,
      data = .sheet_mismatch$rows,
      concept_var = .wb_concept_var,
      concept_color = concept_color,
      text_var = "match",
      text_color = text_color,
      visible = .sheet_mismatch$visible
    ) |>
    wb_add_custom(
      sheet = data_sheets$params$name,
      data = data_sheets$params$data,
      halign = "left"
    )

  data_xlsx_tbl <- data_sheets |> list_transpose() |> pluck("data")

  xlsx_output |> wb_save(file = glue("{save_extract}.xlsx"))

  data_xlsx_gt <-
  list2("{data_sheets$data$name}" :=
          list(
            id =
              data_sheets$data$data |>
                select(-c(matches(concepts_root), concept, extract)) |>
                gt_custom(font_size = 11) |>
                sub_missing() |>
                gt_text_align(),
            text =
              data_sheets$data$data |>
                select(n, id, matches(concepts_root), concept, extract) |>
                mutate(` ... ` = "...", .after = id) |>
                gt_custom(font_size = 11) |>
                gt_text_align() |>
                gt_text_color(column = matches(c(concepts_root, "concept")),
                              color = concept_color) |>
                gt_text_color(column = "extract",
                              color = text_color)
          ),
        "{data_sheets$token_regex$name}" :=
          data_sheets$token_regex$data |>
            gt_custom(head = NULL) |>
            gt_text_color(column = matches("concept"),
                          color = concept_color) |>
            gt_text_color(column = "regex",
                          color = text_color) |>
            gt_code_font(column = "regex"),
        "{data_sheets$token_match$name}" :=
          data_sheets$token_match$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = text_input,
                          color = text_color),
        "{data_sheets$token_count$name}" :=
          data_sheets$token_count$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = text_input,
                          color = text_color),
        "{data_sheets$concept_count$name}" :=
          data_sheets$concept_count$data |>
            gt_custom() |>
            sub_missing() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color),
        "{data_sheets$text_replace$name}" :=
          data_sheets$text_replace$data |>
            gt_custom(head = NULL) |>
            gt_code_font(),
        "{data_sheets$text_regex$name}" :=
          data_sheets$text_regex$data |>
            gt_custom(head = NULL) |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = text_input,
                          color = text_color) |>
            gt_code_font(column = text_input),
        "{data_sheets$text_match$name}" :=
          data_sheets$text_match$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = "match",
                          color = text_color),
        "{data_sheets$text_count$name}" :=
          data_sheets$text_count$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = "match",
                          color = text_color),
        "{data_sheets$mismatch$name}" :=
          data_sheets$mismatch$data |>
            gt_custom() |>
            gt_text_align() |>
            gt_text_color(column = "concept",
                          color = concept_color) |>
            gt_text_color(column = "match",
                          color = text_color) |>
            sub_missing(),
        "{data_sheets$params$name}" :=
          data_sheets$params$data |>
            gt_custom(head = NULL))

### CSV DATA -------------------------------------------------------------------

  cli_progress_step("{.strong Enregistrement du .csv}")
  cli_text("\n\n")

  data_csv <-
  data_extract |>
    set_data_csv(
      text_input = text_input,
      text_color = text_color,
      text_background = text_background,
      pattern = data_regex_str
    ) |>
    select(-matches(concepts_root))

  write_excel_csv(data_csv, file = glue("{save_extract}.csv"))

### ASSIGN DATA ----------------------------------------------------------------

  cli_progress_step("{.strong Enregistrement du .rds}")
  cli_text("\n\n")

  data_save <- list(
    regex = list(
      concepts = concepts_regex_df,
      replace = regex_replace_df,
      final = data_regex_df,
      match = data_regex_match
    ),
    match = list(
      init = data_match,
      final = data_match_final
    ),
    count = list(
      init = data_token_match,
      final = data_count
    ),
    exclus = list(
      match = data_match_exclus,
      count = data_count_exclus
    ),
    mismatch = data_mismatch,
    summary = data_summary,
    data = list(
      base = data |> select(-text_input),
      match = data_match_init_df,
      extract = data_extract,
      xlsx = list(
        tbl = data_xlsx_tbl,
        gt = data_xlsx_gt
      ),
      csv = data_csv
    )
  )

  readr::write_rds(
    x = data_save,
    file = save_extract_rds
  )

  ### PRINT ----------------------------------------------------------------------

  cli_progress_done()

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  data_print <- list(
    token = data_token_match,
    count = list(
      total = data_save$count$final,
      token = data_summary$token,
      concepts = data_summary$concept,
      exclus = data_save$exclus$count,
      final = data_save$count$final
    ),
    regex = data_save$regex,
    mismatch = data_save$mismatch,
    params = data_summary$params
  )

  print(data_print)

  cli_rule()
  cli_text("\n\n")

### CLI ------------------------------------------------------------------------

  toc()
  cli_text("\n\n")

  n_concepts <- length(concepts_root)

  cli_intersect <- if (intersect) " \u00e0 l'intersection {str_concepts_inter}" else ""

  cli_n_id <- nrow(data_id |> filter(.data[[id]] %in% .match_id[[id]]))
  cli_p_id <- label_percent(0.1)(nrow(data) / nrow_init)

  cli_n_match <- n_distinct(data_match_init[[id]])
  cli_p_match <- label_percent(0.1)(cli_n_match / nrow(data))

  cli_n_extract <- nrow(data_extract)
  cli_p_extract <- label_percent(0.1)(cli_n_extract / nrow(data))

  cli_n_group <- n_distinct(data_extract[[group]])
  cli_p_group <- label_percent(0.1)(cli_n_group / n_distinct(data[[group]]))

  cli_n_mismatch <- nrow(data_id_mismatch)
  cli_p_mismatch <- label_percent(0.1)(cli_n_mismatch / nrow(data))

  cli_n_group_mismatch <- n_distinct(data_id_mismatch[[group]])
  cli_p_group_mismatch <- label_percent(0.1)(cli_n_group_mismatch / n_distinct(data[[group]]))

  cli_n_files <- sum(str_detect(list.files(save_dir), "\\.\\w+"))

  cli_col <- \(x) col_blue(glue(x))

  cli_alert_info("{.strong Documents}")
  cli_ul()
  cli_ul()
    cli_li("Total : {nrow_init} {id}")
    if (!is.null(sample)) cli_li("Sample : {nrow(data)} {id} ({cli_p_id})")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Correspondances totales}")
  cli_ul()
    cli_li("{nrow(data_match_init)} parmi {cli_n_match} {id} ({cli_p_match} {id})")
    if (nrow(data_count_exclus) == 0) cli_li("Aucune exclusion")
    cli_end()

  if (nrow(data_count_exclus) > 0) {

    cli_text("\n\n")
    cli_alert_info("{.strong Exclusions}")
    cli_ul()
      cli_li("Automatiques : {nrow(data_match_exclus$auto)}")
      cli_li("Manuelles : {nrow(data_match_exclus$manual)}")
      cli_end()

  }

  if (mismatch_data) {

    cli_text("\n\n")
    cli_alert_info("{.strong Mismatch :} {cli_n_mismatch} {id}")

  }

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {n_concepts} concept{?s} parent{?s} :} {str_concepts_comma}")
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_id} correspondance{?s}{glue(cli_intersect)}}")
  cli_ul()
    cli_li("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (!is.null(which_group)) cli_li("{cli_n_group} {group} ({cli_p_group} {group})")
    cli_end()

  if (mismatch_data && cli_n_mismatch > 0) {

    cli_text("\n\n")
    cli_alert_success("{.strong {cli_n_mismatch} mismatch{?es}}")
    cli_ul()
      cli_li("{cli_n_mismatch} {id} ({cli_p_mismatch} {id})")
      if (!is.null(which_group)) cli_li("{cli_n_group_mismatch} {group} ({cli_p_group_mismatch} {group})")
      cli_end()

  }

  cli_text("\n\n")
  cli_alert_success("{.strong {nrow(data_count)} correspondance{?s} distincte{?s}}")

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {cli_n_files} fichier{?s} enregistr\u00e9{?s} dans le r\u00e9pertoire {.path {save_dir}}}")
  cli_ul()
    cli_li("rds : {cli_col('{save_files}.rds')}")
    cli_li("xlsx : {cli_col('{save_files}.xlsx')}")
    cli_li("csv : {cli_col('{save_files}.csv')}")
    cli_end()

  cli_text("\n\n")
  cli_rule()

  invisible(gc())

  return(invisible(data_save))

  } else {

    cli_load(
      dir = save_dir,
      file = save_files,
      save = save_extract_rds
    )

  }

}
