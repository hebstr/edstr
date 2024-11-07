.extract_output <- \(highlight_weight,
                     highlight_color,
                     highlight_bg,
                     data_str_br,
                     data_extract,
                     data_count,
                     data_match_list,
                     text_input,
                     html_popup,
                     .save_extract) {

  cli_progress_step("{.strong Set output}")

  h_weight <- glue("font-weight:{highlight_weight}")
  h_color <- glue("color:{highlight_color}")
  h_bg <- glue("background-color:{highlight_bg}")

  rt_common <-
  list(args =
         list(height = "100%",
              defaultColDef =
                colDef(vAlign = "center",
                       align = "center"),
              showSortable = TRUE,
              striped = TRUE,
              searchable = TRUE,
              filterable = TRUE),
       theme =
         list(style =
                list(fontFamily = "Luciole, Trebuchet MS",
                     fontSize = "12px"),
              borderColor = "#dfe2e5",
              searchInputStyle = list(width = "100%")))

  .data_text <-
  data_str_br |>
    mutate(!!text_input :=
             get(text_input) |>
               str_replace_all(glue("(?={data_extract})"),
                               glue("<span style='{h_weight};{h_color};{h_bg}'> ")) |>
               str_replace_all(glue("(?<={data_extract})"),
                               "</span></span>")) |>
    select(-nchar) |>
    reactable(!!!rt_common$args,
              selection = "multiple",
              columns =
                list2(.selection = colDef(sticky = "left"),
                      .rownames =
                        colDef(name = "n°",
                               width = 40,
                               sticky = "left"),
                      !!text_input :=
                        colDef(html = TRUE,
                               minWidth = 500,
                               maxWidth = 700,
                               style = list(textAlign = "justify"))),
              rownames = TRUE,
              highlight = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100, 200, 500, 1000),
              defaultPageSize = 100,
              theme =
                reactableTheme(!!!rt_common$theme,
                               rowSelectedStyle = list(backgroundColor = "skyblue"))) |>
    inject()

  .data_cpts <-
  data_count |>
    select(-ngrams) |>
    reactable(!!!rt_common$args,
              columns =
                list2(!!text_input :=
                        colDef(name = "expression",
                               minWidth = 700,
                               style = list(color = highlight_color)),
                      concept = colDef(minWidth = 300),
                      n = colDef(maxWidth = 75)),
              defaultPageSize = 1000,
              theme = reactableTheme(!!!rt_common$theme)) |>
    inject()

  data_to_output <- \(x, name) {

    y <- enexpr(x)

    output <- glue("{.save_extract}_{name}.html")

    save_html(x, output)

    if (html_popup) browseURL(output)

    assign(glue(y), x, envir = .GlobalEnv)

  }

  data_to_output(.data_cpts, name = "concepts")
  data_to_output(.data_text, name = "text")

  cli_progress_done()
  cli_text("\n\n")

}
