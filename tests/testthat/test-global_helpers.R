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
