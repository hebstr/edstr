.extract_print_summary <- \(
  data_save,
  data,
  data_id,
  data_match_init,
  match_id,
  concepts_list,
  nrow_init,
  id,
  group,
  which_group,
  sample,
  intersect,
  unmatched_data,
  save_dir,
  save_files,
  format_drops
) {
  data_extract <- data_save$data$extract
  data_count <- data_save$count$final
  data_count_exclus <- data_save$exclus$count
  data_match_exclus <- data_save$exclus$match
  data_unmatched <- data_save$unmatched
  data_mismatched <- data_save$mismatched

  n_concepts <- length(concepts_list$root)

  cli_intersect <- if (intersect) {
    " at intersection {concepts_list$str$inter}"
  } else {
    ""
  }

  cli_n_id <- nrow(data_id |> filter(.data[[id]] %in% match_id[[id]]))
  cli_p_id <- label_pct(nrow(data) / nrow_init)

  cli_n_match <- n_distinct(data_match_init[[id]])
  cli_p_match <- label_pct(cli_n_match / nrow(data))

  cli_n_extract <- nrow(data_extract)
  cli_p_extract <- label_pct(cli_n_extract / nrow(data))

  cli_alert_info("{.strong Documents}")
  cli_ul("Total: {nrow_init} {id}")
  if (!is.null(sample)) {
    cli_ul("Sample: {nrow(data)} {id} ({cli_p_id})")
  }
  .extract_print_drops(format_drops)

  br()
  cli_alert_info("{.strong Total matches}")
  cli_ul(
    "{nrow(data_match_init)} across {cli_n_match} {id} ({cli_p_match} {id})"
  )
  if (nrow(data_count_exclus) == 0) {
    cli_ul("No exclusions")
  }

  if (nrow(data_count_exclus) > 0) {
    br()
    cli_alert_info("{.strong Exclusions}")
    cli_ul("Automatic: {nrow(data_match_exclus$auto)}")
    cli_ul("Manual: {nrow(data_match_exclus$manual)}")
  }

  if (unmatched_data) {
    cli_n_unmatched <- nrow(data_unmatched$no_concept)
    cli_p_unmatched <- label_pct(cli_n_unmatched / nrow(data))

    br()
    cli_alert_info("{.strong Unmatched (no concept):} {cli_n_unmatched} {id}")
  }

  br()
  cli_rule()
  br()

  cli_alert_success(
    "{.strong {n_concepts} root concept{?s}:} {concepts_list$str$comma}\n\n"
  )

  cli_alert_success("{.strong {cli_n_id} match{?es}{glue(cli_intersect)}}")
  cli_ul("{cli_n_extract} {id} ({cli_p_extract} {id})")
  if (!is.null(which_group)) {
    cli_n_group <- n_distinct(data_extract[[group]])
    cli_p_group <- label_pct(cli_n_group / n_distinct(data[[group]]))
    cli_ul("{cli_n_group} {group} ({cli_p_group} {group})")
  }

  if (unmatched_data && cli_n_unmatched > 0) {
    br()
    cli_alert_success("{.strong {cli_n_unmatched} unmatched (no concept)}")
    cli_ul("{cli_n_unmatched} {id} ({cli_p_unmatched} {id})")
    if (!is.null(which_group)) {
      cli_n_group_unmatched <- n_distinct(data_unmatched$no_concept[[group]])
      cli_p_group_unmatched <- label_pct(
        cli_n_group_unmatched / n_distinct(data[[group]])
      )
      cli_ul("{cli_n_group_unmatched} {group} ({cli_p_group_unmatched} {group})")
    }
  }

  if (nrow(data_mismatched) > 0) {
    br()
    cli_alert_warning(
      "{.strong {nrow(data_mismatched)} source mismatch{?es}} (token matched, not confirmed in source)"
    )
  }

  br()
  cli_alert_success("{.strong {nrow(data_count)} distinct match{?es}}")

  br()
  cli_rule()
  br()

  cli_alert_info("{.strong Root directory: {.path {here::here()}}}\n\n")

  cli_path <- fs::path(save_dir)

  cli_alert_success("{.strong Files saved in {cli::col_blue(cli_path)}}")

  br()
  cli_rule()
}
