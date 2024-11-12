#' Title
#'
#' @param data
#' @param count_min
#' @param id
#' @param config
#' @param text_input
#' @param pattern
#' @param word_max
#'
#' @return
#' @export
#'
#' @examples
#'
edstr_split <- \(data,
                 pattern,
                 start = "upper",
                 word_max = 2,
                 count_min = 100,
                 id,
                 str,
                 config = get(.config_name),
                 text_input = with(config, text)) {

  if (!exists(".config_name")) {

    config <- cli_error_config()

  } else config <- get(.config_name)

  if (is.character(data)) data <- get(data)
  if (is.character(config)) config <- get(config)

  config_dir <- with(config, dir)
  config_file <- glue("{with(config, file)}_split")
  config_save <- glue("{config_dir}/{config_file}.RData")

### SPLIT_FUN -----------------------------------------------------------------------------

  split_fun <- \(.pattern, .id) {

    if (!is.null(names(.pattern))) {

      split_name <- glue("_{names(.pattern)}")
      split_mode <- names(.pattern)

    } else cli_abort("pattern argument must be named")

    config_input <- glue("{with(config, file)}_view{split_name}")

    edstr_view(data = data,
               str = .pattern,
               id = .id,
               text_input = text_input)

    split_data <-
    get(config_input) |>
      pluck("all_matches") |>
      select(.id, match) |>
      arrange(get(.id))

    cli_text("\n\n")
    cli_alert_info(glue("split mode: {split_mode}"))
    cli_text("\n\n")
    cli_alert_success("{.strong {config_input}} created")
    cli_text("\n\n")
    cli_rule()

    assign(glue(config_input),
           split_data,
           envir = .GlobalEnv)

  }

  .split_data <-
  map(1:length(pattern),
      ~ split_fun(.pattern = pattern[.], .id = id))

### BIND ROWS ---------------------------------------------------------------------------------

  cli_h1("edstr_split")
  cli_text("\n\n")


  split_command <- \(x) {

    x |>
      mutate(match =
               match |>
                 str_extract(glue("\\S+(\\s+\\S+){{0,{word_max-1}}}")) |>
                 str_replace_all(c("^\\s+|\\s*\\W+\\s*$" = "",
                                   "\\d+" = "\\\\d+",
                                   "(?=(\\(|\\)|\\*|\\.|\\?))" = "\\\\"))) |>
      count(match, sort = TRUE) |>
      filter(str_starts(match, glue("[:{start}:]")),
             !str_detect(match, "^[:upper:]+$"),
             !str_detect(match, "^M(\\.|(?i)(onsi|ada|r|me|lle|e))"),
             n >= count_min)

  }

  if (length(pattern) > 1) {

    if (!is.null(names(pattern))) {

      split_name_flatten <- str_flatten_comma(names(pattern), " and ")

    } else split_name_flatten <- str_flatten_comma(pattern, " and ")

    cli_progress_step("Binding {split_name_flatten} rows")

    .split_bind <-
    .split_data |>
      bind_rows() |>
      split_command()

  } else {

    .split_bind <-
    .split_data |>
      list_c() |>
      split_command()

  }

  .split_bind_str <- str_c(.split_bind$match, collapse = "|")
  .split_str <- glue(enexpr(str))

### SAVE ---------------------------------------------------------------------------------

  cli_progress_step("Saving {.strong {config_file}}")

  assign(config_file,
         list(bind = .split_bind,
              str = .split_str),
         envir = .GlobalEnv)

  save(list = config_file, file = config_save)

  cli_progress_done()

### CLI ---------------------------------------------------------------------------------

  cli_text("\n\n")
  cli_alert_success("{.strong {config_file}} saved to {.path {config_save}}")
  cli_text("\n\n")
  cli_rule()

}
