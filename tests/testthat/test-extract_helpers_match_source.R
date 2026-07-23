test_that("match_source: regex_replace widens what the source matches", {
  data_match_df <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = "displasie moderee",
    stringsAsFactors = FALSE
  )

  data_count <- data.frame(
    concept = "lesion",
    texte = "dysplasie",
    stringsAsFactors = FALSE
  )

  match_source <- \(regex_replace) {
    edstr:::.extract_match_source(
      data_match_df = data_match_df,
      data_count = data_count,
      text_input = "texte",
      id = "doc_id",
      regex_replace = regex_replace
    )
  }

  without <- match_source(NULL)
  with <- match_source(c("y" = "[yi]"))

  expect_equal(nrow(without$data_regex_match), 0)

  expect_true("y" %in% with$regex_replace_df$pattern)
  expect_equal(with$data_regex_match$match, "displasie")
})
