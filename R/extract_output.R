.extract_output <- \(highlight_color,
                     highlight_bg,
                     data_str_br,
                     data_extract,
                     data_count,
                     data_match_list,
                     text_input,
                     .save_extract) {

  cli::cli_progress_step("{.strong Set output}")

  h_color <- glue::glue("color:{highlight_color}")
  h_bg <- glue::glue("background-color:{highlight_bg}")

  rt_common <-
  list(args = list(height = "100%",
                   defaultColDef = reactable::colDef(vAlign = "center",
                                                     align = "center"),
                   showSortable = TRUE,
                   striped = TRUE,
                   searchable = TRUE,
                   filterable = TRUE),
       theme = list(style = list(fontFamily = "Segoe UI",
                                 fontSize = "11px"),
                    borderColor = "#dfe2e5",
                    stripedColor = "#e1f6ff",
                    searchInputStyle = list(width = "100%")))

  .data_text <-
  data_str_br |>
    dplyr::mutate(!!text_input :=
                    get(text_input) |>
                      stringr::str_replace_all(glue::glue("(?={data_extract})"),
                                               glue::glue("<span style='{h_color};{h_bg}'> ")) |>
                      stringr::str_replace_all(glue::glue("(?<={data_extract})"),
                                               "</span></span>")) |>
    dplyr::select(-nchar) |>
    reactable::reactable(!!!rt_common$args,
                         selection = "multiple",
                         columns = rlang::list2(.selection = reactable::colDef(sticky = "left"),
                                                .rownames = reactable::colDef(name = "no",
                                                                              width = 40,
                                                                              sticky = "left"),
                                                !!text_input := reactable::colDef(html = TRUE,
                                                                                  minWidth = 500,
                                                                                  maxWidth = 500,
                                                                                  style = list(textAlign = "justify"))),
                         rownames = TRUE,
                         highlight = TRUE,
                         showPageSizeOptions = TRUE,
                         pageSizeOptions = c(10, 25, 50, 100, 200, 500, 1000),
                         defaultPageSize = 100,
                         theme = reactable::reactableTheme(!!!rt_common$theme,
                                                           rowSelectedStyle = list(backgroundColor = "skyblue"))) |> rlang::inject()

  .data_cpts <-
  data_count |>
    dplyr::select(-ngrams) |>
    reactable::reactable(!!!rt_common$args,
                         columns = rlang::list2(.rownames = reactable::colDef(name = "no",
                                                                              width = 40,
                                                                              sticky = "left"),
                                                !!text_input := reactable::colDef(name = "expression",
                                                                                  style = list(color = highlight_color)),
                                                concept = reactable::colDef(maxWidth = 200),
                                                n = reactable::colDef(maxWidth = 75)),
                         defaultPageSize = 1000,
                         theme = reactable::reactableTheme(!!!rt_common$theme)) |> rlang::inject()

  data_to_output <- \(x, name) {

    y <- rlang::enexpr(x)

    output <- glue::glue("{.save_extract}_{name}.html")

    htmltools::save_html(x, output)

    utils::browseURL(output)

    assign(glue::glue(y), x, envir = .GlobalEnv)

  }

  data_to_output(.data_cpts, name = "concepts")
  data_to_output(.data_text, name = "text")

  cli::cli_progress_done()
  cli::cli_text("\n\n")

}
