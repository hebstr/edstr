########################################

  #message(glue::glue(
  "n total: {n <- nrow(data_total)} obs.
  n sample: {sample} obs. ({round(sample/n*100, 1)}% total)
  soit n base: {nt <- nrow(data)} obs. ({round(nt/sample*100, 1)}% sample, {round(nt/n*100, 1)}% total)

  {nrow(data_match)} matchs pour {nm <- dplyr::n_distinct(data_match[[v <- join_by]])} {v} ({round(nm/nt*100, 1)}% total)
  {dplyr::n_distinct(data_match[[text_input]])} expressions distinctes
  {nrow(data_exclus_auto)} exclusions auto
  {nrow(data_exclus_man)} exclusions manuelles
  {db <- nrow(data_str_br_db) - nrow(data_str_br)} doublons ({round(db/nrow(data_str_br_db)*100, 1)}% base)
  {nrow(data_exclus_nchar)} extraits > {nchar_max} chr

  soit:
  {nrow(data_id)} matchs pour {ni <- dplyr::n_distinct(data_str_br[[v]])} {v} ({round(ni/nt*100, 1)}% base)
  {nrow(data_count)} expressions distinctes pour {dplyr::n_distinct(data_count$concept)} concepts
  {dplyr::n_distinct(data_str_br[[group_by]])} {group_by}\n\n"
  #))


#######################################
