.extract_cli <- \(data,
                  data_total,
                  data_match,
                  data_id,
                  data_count,
                  data_str_br,
                  data_str_br_db,
                  data_exclus_auto,
                  data_exclus_man,
                  data_exclus_nchar,
                  text_input,
                  nchar_max,
                  id,
                  group,
                  save_dir) {

  cli_p_final <- scales::percent(nrow(data) / nrow(data_total), accuracy = 0.1)
  cli_n_join <- dplyr::n_distinct(data_match[[id]])
  cli_p_join <- scales::percent(cli_n_join / nrow(data), accuracy = 0.1)
  cli_n_distinct_match <- dplyr::n_distinct(data_match[[text_input]])
  cli_n_dupl <- nrow(data_str_br_db) - nrow(data_str_br)
  cli_n_extract <- dplyr::n_distinct(data_str_br[[id]])
  cli_p_extract <- scales::percent(cli_n_extract / nrow(data), accuracy = 0.1)
  cli_n_concept <- dplyr::n_distinct(data_count$concept)
  cli_n_group <- dplyr::n_distinct(data_str_br[[group]])

  cli::cli_text("\n\n")
  cli::cli_rule()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Observations}")
  cli::cli_ul()
    cli::cli_li("Total: {nrow(data_total)}")
    cli::cli_li("Final: {nrow(data)} ({cli_p_final})")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Matching}")
  cli::cli_ul()
    cli::cli_li("Total: {nrow(data_match)} from {cli_n_join} {id} ({cli_p_join} final obs.)")
    cli::cli_li("Distinct: {cli_n_distinct_match}")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Exclusions}")
  cli::cli_ul()
    cli::cli_li("Auto: {nrow(data_exclus_auto)}")
    cli::cli_li("Manual: {nrow(data_exclus_man)}")
    cli::cli_li("Duplicates: {cli_n_dupl}")
    cli::cli_li("Over {nchar_max} chr.: {nrow(data_exclus_nchar)}")
    cli::cli_end()

  cli::cli_text("\n\n")
  cli::cli_rule()
  cli::cli_text("\n\n")
  cli::cli_alert_success("{nrow(data_id)} matchs from:")
  cli::cli_ul()
    cli::cli_li("{cli_n_extract} {id} ({cli_p_extract} final obs.)")
    cli::cli_li("{cli_n_group} {group}")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_alert_success("{nrow(data_count)} distinct expressions from {cli_n_concept} concepts")

  cli::cli_text("\n\n")
  cli::cli_alert_success("files saved to {.path {save_dir}}")
  cli::cli_text("\n\n")
  cli::cli_rule()

}
