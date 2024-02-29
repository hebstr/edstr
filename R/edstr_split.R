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
edstr_split <- \(data = glue::glue("{with(config, file)}_clean"),
                 pattern,
                 word_max = 2,
                 count_min = 100,
                 id,
                 config = get(.config_name),
                 text_input = with(config, text)) {

  cli_error_config()

  if (is.character(data)) data <- get(data)
  if (is.character(config)) config <- get(config)

  config_dir <- with(config, dir)
  config_file <- glue::glue("{with(config, file)}_split")
  config_save <- glue::glue("{config_dir}/{config_file}.RData")

### SPLIT_FUN -----------------------------------------------------------------------------

  split_fun <- \(.pattern, .id) {

    if (!is.null(names(.pattern))) {

      split_name <- glue::glue("_{names(.pattern)}")
      split_mode <- names(.pattern)

    } else {

      split_name <- ""
      split_mode <- .pattern

    }

    config_input <- glue::glue("{with(config, file)}_view{split_name}")

    edstr_view(data = data,
               str = .pattern,
               id = .id,
               text_input = text_input)

    split_data <-
    get(config_input) |>
      purrr::pluck("all_matches") |>
      dplyr::select(.id, match) |>
      dplyr::arrange(get(.id))

    cli::cli_text("\n\n")
    cli::cli_alert_info(glue::glue("split mode: {split_mode}"))
    cli::cli_text("\n\n")
    cli::cli_alert_success("{.strong {config_input}} created")
    cli::cli_text("\n\n")
    cli::cli_rule()

    assign(glue::glue(config_input),
           split_data,
           envir = .GlobalEnv)

  }

  .split_data <-
  list(split_fun(.pattern = pattern[1], .id = id),
       split_fun(.pattern = pattern[2], .id = id))

### BIND ROWS ---------------------------------------------------------------------------------

  if (!is.null(names(pattern))) {

   split_name_flatten <- stringr::str_flatten(names(pattern), " and ")

  } else split_name_flatten <- stringr::str_flatten(pattern, " and ")

  cli::cli_h1("edstr_split")
  cli::cli_text("\n\n")
  cli::cli_progress_step("Binding {split_name_flatten} rows")

  .split_bind <-
  .split_data |>
    dplyr::bind_rows() |>
    dplyr::mutate(match =
                    match |>
                      stringr::str_extract(glue::glue("\\S+(\\s+\\S+){{0,{word_max-1}}}")) |>
                      stringr::str_replace_all(c("^\\s+|\\s*\\W+\\s*$" = "",
                                                 "\\d+" = "\\\\d+",
                                                 "(?=(\\(|\\)|\\*|\\.|\\?))" = "\\\\"))) |>
    dplyr::count(match, sort = TRUE) |>
    dplyr::filter(stringr::str_starts(match, "[:upper:]"),
                  !stringr::str_detect(match, "^[:upper:]+$"),
                  !stringr::str_detect(match, "^M(\\.|(?i)(onsi|ada|r|me|lle|e))"),
                  n >= count_min)

  .split_bind_str <- stringr::str_c(.split_bind$match, collapse = "|")
  .split_str <- glue::glue("(?<=>)(\\s*|\\d(\\.|\\)|/)\\s*)?(?=({.split_bind_str}))")

### SAVE ---------------------------------------------------------------------------------

  cli::cli_progress_step("Saving {.strong {config_file}}")

  assign(config_file,
         list(bind = .split_bind,
              str = .split_str),
         envir = .GlobalEnv)

  save(list = config_file, file = config_save)

  cli::cli_progress_done()

### CLI ---------------------------------------------------------------------------------

  cli::cli_text("\n\n")
  cli::cli_alert_success("{.strong {config_file}} saved to {.path {config_save}}")
  cli::cli_text("\n\n")
  cli::cli_rule()

}
