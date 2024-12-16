.extract_cli <- \(data,
                  data_total,
                  data_match,
                  data_id,
                  data_count,
                  data_str_br,
                  data_str_br_db,
                  data_count_exclus,
                  data_exclus_nchar,
                  text_input,
                  nchar_max,
                  id,
                  group,
                  save_dir) {

  cli_p_final <- label_percent(0.1)(nrow(data) / nrow(data_total))
  cli_n_join <- n_distinct(data_match[[id]])
  cli_p_join <- label_percent(0.1)(cli_n_join / nrow(data))
  cli_n_distinct_match <- n_distinct(data_match[[text_input]])
  cli_n_dupl <- nrow(data_str_br_db) - nrow(data_str_br)
  cli_n_extract <- n_distinct(data_str_br[[id]])
  cli_p_extract <- label_percent(0.1)(cli_n_extract / nrow(data))
  cli_n_concept <- n_distinct(data_count$concept)
  cli_n_group <- n_distinct(data_str_br[[group]])

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_info("{.strong Observations}")
  cli_ul()
  cli_ul()
    cli_li("Total: {nrow(data_total)} {id}")
    cli_li("Sample: {nrow(data)} ({cli_p_final})")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Matching}")
  cli_ul()
    cli_li("Total: {nrow(data_match)} from {cli_n_join} {id} ({cli_p_join} {id})")
    cli_li("Distinct: {cli_n_distinct_match}")
    cli_end()

  cli_text("\n\n")
  cli_alert_info("{.strong Exclusions}")
  cli_ul()
    cli_li("Auto: {nrow(data_exclus_auto)}")
    cli_li("Manual: {nrow(data_exclus_man)}")
    cli_li("Duplicated: {cli_n_dupl}")
    cli_li("Over character limit (n={nchar_max}): {nrow(data_exclus_nchar)}")
    cli_end()

  cli_text("\n\n")
  cli_rule()
  cli_text("\n\n")

  cli_alert_success("{.strong {nrow(data_id)} matchs from}")
  cli_ul()
    cli_li("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (group != "group_number") cli_li("{cli_n_group} {group}")
    cli_end()

  cli_text("\n\n")
  cli_alert_success("{.strong {nrow(data_count)} distinct expressions for {cli_n_concept} concepts}")

  cli_text("\n\n")
  cli_alert_success("files saved to {.path {save_dir}}")

  cli_text("\n\n")
  cli_rule()

}
