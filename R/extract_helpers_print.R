.extract_print_summary <- \(
  data_save,
  data, data_id, data_match_init, match_id,
  concepts_list,
  nrow_init, id, group, which_group,
  sample, intersect, mismatch_data,
  save_dir, save_files
) {

  data_extract <- data_save$data$extract
  data_count <- data_save$count$final
  data_count_exclus <- data_save$exclus$count
  data_match_exclus <- data_save$exclus$match
  data_mismatch <- data_save$mismatch

  n_concepts <- length(concepts_list$root)

  cli_intersect <- if (intersect) " \u00e0 l'intersection {concepts_list$str$inter}" else ""

  cli_n_id <- nrow(data_id |> filter(.data[[id]] %in% match_id[[id]]))
  cli_p_id <- label_pct(nrow(data) / nrow_init)

  cli_n_match <- n_distinct(data_match_init[[id]])
  cli_p_match <- label_pct(cli_n_match / nrow(data))

  cli_n_extract <- nrow(data_extract)
  cli_p_extract <- label_pct(cli_n_extract / nrow(data))

  cli_alert_info("{.strong Documents}")
    cli_ul("Total : {nrow_init} {id}")
    if (!is.null(sample)) cli_ul("Sample : {nrow(data)} {id} ({cli_p_id})")

  br(); cli_alert_info("{.strong Correspondances totales}")
    cli_ul("{nrow(data_match_init)} parmi {cli_n_match} {id} ({cli_p_match} {id})")
    if (nrow(data_count_exclus) == 0) cli_ul("Aucune exclusion")

  if (nrow(data_count_exclus) > 0) {

    br(); cli_alert_info("{.strong Exclusions}")
      cli_ul("Automatiques : {nrow(data_match_exclus$auto)}")
      cli_ul("Manuelles : {nrow(data_match_exclus$manual)}")

  }

  if (mismatch_data) {

    cli_n_mismatch <- nrow(data_mismatch$id)
    cli_p_mismatch <- label_pct(cli_n_mismatch / nrow(data))

    br(); cli_alert_info("{.strong Mismatch :} {cli_n_mismatch} {id}")

  }

  br(); cli_rule(); br()

  cli_alert_success("{.strong {n_concepts} concept{?s} parent{?s} :} {concepts_list$str$comma}\n\n")

  cli_alert_success("{.strong {cli_n_id} correspondance{?s}{glue(cli_intersect)}}")
    cli_ul("{cli_n_extract} {id} ({cli_p_extract} {id})")
    if (!is.null(which_group)) {
      cli_n_group <- n_distinct(data_extract[[group]])
      cli_p_group <- label_pct(cli_n_group / n_distinct(data[[group]]))
      cli_ul("{cli_n_group} {group} ({cli_p_group} {group})")
    }

  if (mismatch_data && cli_n_mismatch > 0) {

    br(); cli_alert_success("{.strong {cli_n_mismatch} mismatch{?s}}")
      cli_ul("{cli_n_mismatch} {id} ({cli_p_mismatch} {id})")
      if (!is.null(which_group)) {
        cli_n_group_mismatch <- n_distinct(data_mismatch$id[[group]])
        cli_p_group_mismatch <- label_pct(cli_n_group_mismatch / n_distinct(data[[group]]))
        cli_ul("{cli_n_group_mismatch} {group} ({cli_p_group_mismatch} {group})")
      }

  }

  br(); cli_alert_success("{.strong {nrow(data_count)} correspondance{?s} distincte{?s}}")

  br(); cli_rule(); br()

  cli_alert_info("{.strong R\u00e9pertoire parent : {.path {here::here()}}}\n\n")

  cli_path <- fs::path(save_dir, save_files, ext = "<xlsx/csv/rds>")

  cli_alert_success("{.strong Fichiers enregistr\u00e9s : {cli::col_blue(cli_path)}}")

  br(); cli_rule()

}
