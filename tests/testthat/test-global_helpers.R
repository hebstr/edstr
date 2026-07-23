test_that("read_query: a bare SQL string passes through untouched", {
  expect_equal(
    suppressMessages(edstr:::read_query("SELECT 1 FROM dual")),
    "SELECT 1 FROM dual"
  )
})

test_that("read_query: a missing .sql path errors", {
  expect_error(
    suppressMessages(edstr:::read_query("absent.sql")),
    "not found"
  )
})

test_that("read_query: comments are stripped and the query flattened", {
  path <- withr::local_tempfile(
    fileext = ".sql",
    lines = c(
      "SELECT id, -- inline comment",
      "  note",
      "/* block",
      "   comment */",
      "FROM patients"
    )
  )

  out <- suppressMessages(edstr:::read_query(path))

  expect_no_match(out, "comment")
  expect_false(grepl("\n", out, fixed = TRUE))
  expect_match(out, "^SELECT id,")
  expect_match(out, "FROM patients$")
})

test_that("read_query: a comment-only file errors", {
  path <- withr::local_tempfile(
    fileext = ".sql",
    lines = c("-- only a comment", "/* and a block */")
  )

  expect_error(
    suppressMessages(edstr:::read_query(path)),
    "empty or contains only comments"
  )
})


test_that("view_output: error_empty = FALSE returns an empty result set", {
  out <- edstr:::view_output(
    data = data.frame(id = 1, note = "rien ici"),
    text_input = "note",
    pattern = "introuvable",
    id = "id",
    error_empty = FALSE
  )

  expect_named(out, c("match", "count", "text"))
  expect_equal(nrow(out$match), 0)
  expect_equal(nrow(out$count), 0)
  expect_equal(out$text, character())
})

test_that("pua_guard: errors when every candidate plane is occupied", {
  occupied <- paste0(
    intToUtf8(0xF0000),
    intToUtf8(0x100000),
    intToUtf8(0xE000)
  )

  expect_error(
    edstr:::.pua_guard(occupied, 2L),
    "No free private-use range"
  )
  expect_length(edstr:::.pua_guard("texte normal", 2L), 2L)
})


test_that("set_class_css: wraps a match in a concept-classed span", {
  out <- edstr:::set_class_css(
    "cancer du sein",
    list(tumeur = "(?i)\\b(cancer)\\b")
  )

  expect_equal(out, "<span class='extract tumeur'>cancer</span> du sein")
})

test_that("set_class_css: nested concept names become a class hierarchy", {
  out <- edstr:::set_class_css(
    "cancer",
    list(root_diag = "(?i)\\b(cancer)\\b")
  )

  expect_equal(out, "<span class='extract root root-diag'>cancer</span>")
})

test_that("set_class_css: a later concept does not annotate injected markup", {
  out <- edstr:::set_class_css(
    "le patient a un cancer du sein",
    list(
      cancer = "(?i)\\b(cancer)\\b",
      extract = "(?i)\\b(extract|sein)\\b"
    )
  )

  expect_equal(
    out,
    paste0(
      "le patient a un <span class='extract cancer'>cancer</span> du ",
      "<span class='extract extract'>sein</span>"
    )
  )
  expect_false(grepl("class='<span", out, fixed = TRUE))
})

test_that("set_class_css: markup words in a class are not re-annotated", {
  out <- edstr:::set_class_css(
    "un span isole",
    list(balise = "(?i)\\b(span)\\b", autre = "(?i)\\b(class|extract)\\b")
  )

  expect_equal(out, "un <span class='extract balise'>span</span> isole")
})

test_that("set_class_css: overlapping concepts still nest", {
  out <- edstr:::set_class_css(
    "cancer",
    list(a = "(?i)\\b(cancer)\\b", b = "(?i)\\b(cancer)\\b")
  )

  expect_equal(
    out,
    paste0(
      "<span class='extract a'><span class='extract b'>cancer</span></span>"
    )
  )
})

test_that("set_class_css: rows restricts a pass without changing its result", {
  text <- c("cancer du sein", "diabete de type 2", "cancer du poumon")
  pattern <- list(
    tumeur = "(?i)\\b(cancer)\\b",
    endoc = "(?i)\\b(diabete)\\b"
  )

  rows <- list(tumeur = c(TRUE, FALSE, TRUE), endoc = c(FALSE, TRUE, FALSE))

  expect_equal(
    edstr:::set_class_css(text, pattern, rows = rows),
    edstr:::set_class_css(text, pattern)
  )
})

test_that("set_class_css: rows gates each pass independently", {
  text <- c("cancer du sein", "diabete de type 2")
  pattern <- list(
    tumeur = "(?i)\\b(cancer)\\b",
    endoc = "(?i)\\b(diabete)\\b"
  )

  out <- edstr:::set_class_css(
    text,
    pattern,
    rows = list(tumeur = c(FALSE, FALSE), endoc = c(FALSE, TRUE))
  )

  expect_equal(out[[1]], "cancer du sein")
  expect_equal(out[[2]], "<span class='extract endoc'>diabete</span> de type 2")
})

test_that("set_class_css: private-use characters in source survive markup", {
  pua <- intToUtf8(0xF0A7)
  text <- paste0("cancer ", pua, " suite")

  out <- edstr:::set_class_css(text, list(tumeur = "(?i)\\b(cancer)\\b"))

  expect_equal(
    out,
    paste0("<span class='extract tumeur'>cancer</span> ", pua, " suite")
  )
})

test_that("set_class_css: an empty pattern set leaves the text untouched", {
  expect_equal(edstr:::set_class_css("cancer", list()), "cancer")
})
