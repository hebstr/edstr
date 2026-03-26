test_that("edstr_view() errors when data is not a data.frame", {
  expect_error(
    edstr_view(data = "not_df", pattern = "abc"),
    "data.frame"
  )
})

test_that("edstr_view() errors when text_input is not character", {
  withr::local_options(edstr_text = 123)

  expect_error(
    edstr_view(data = data.frame(id = 1, note = "x"), pattern = "x"),
    "character"
  )
})

test_that("edstr_view() errors when pattern is not character", {
  withr::local_options(edstr_text = "note")

  expect_error(
    edstr_view(
      data = data.frame(id = 1, note = "x"),
      pattern = 123
    ),
    "character"
  )
})

test_that("edstr_view() extracts matching tokens and returns results", {
  withr::local_options(edstr_text = "note")

  input <- data.frame(
    doc_id = 1:4,
    note = c(
      "patient avec diabete type 2",
      "controle normal sans diabete",
      "diabete insulino dependant",
      "pas de pathologie"
    )
  )

  result <- suppressMessages(
    edstr_view(data = input, pattern = "diabete")
  )

  expect_type(result, "list")
  expect_named(result, c("match", "count", "text"))
  expect_s3_class(result$match, "data.frame")
  expect_s3_class(result$count, "data.frame")
  expect_contains(names(result$match), c("doc_id", "match"))
  expect_equal(nrow(result$match), 3)
  expect_in(result$match$doc_id, c(1, 2, 3))
})

test_that("edstr_view() errors when no match is found", {
  withr::local_options(edstr_text = "note")

  input <- data.frame(id = 1, note = "rien ici")

  expect_error(
    suppressMessages(
      edstr_view(data = input, pattern = "introuvable")
    ),
    "Aucune correspondance"
  )
})

test_that("edstr_view() applies replacements before matching", {
  withr::local_options(edstr_text = "note")

  input <- data.frame(id = 1, note = "foo bar")

  result <- suppressMessages(
    edstr_view(
      data = input,
      replace = c("foo" = "target"),
      pattern = "target"
    )
  )

  expect_equal(nrow(result$match), 1)
  expect_equal(result$match$match, "target")
})

test_that("edstr_view() supports ngrams > 1", {
  withr::local_options(edstr_text = "note")

  input <- data.frame(id = 1, note = "diabete type 2 confirmed")

  result_1 <- suppressMessages(
    edstr_view(data = input, pattern = "diabete", ngrams = 1)
  )
  result_3 <- suppressMessages(
    edstr_view(data = input, pattern = "diabete", ngrams = 3)
  )

  expect_equal(result_1$match$match, "diabete")
  expect_equal(result_3$match$match, "diabete type 2")
})

test_that("edstr_view() auto-detects id column", {
  withr::local_options(edstr_text = "note")

  input <- data.frame(
    patient_id = c(1, 2, 3),
    note = c("diabete", "normal", "diabete")
  )

  result <- suppressMessages(
    edstr_view(data = input, pattern = "diabete")
  )

  expect_true("patient_id" %in% names(result$match))
})

test_that("edstr_view() handles multiple matches within a single document", {
  withr::local_options(edstr_text = "note")

  input <- data.frame(
    doc_id = 1,
    note = "diabete type 2 et diabete gestationnel"
  )

  result <- suppressMessages(
    edstr_view(data = input, pattern = "diabete")
  )

  expect_equal(nrow(result$match), 2)
  expect_equal(nrow(result$count), 1)
  expect_equal(result$count$n, 2)
})
