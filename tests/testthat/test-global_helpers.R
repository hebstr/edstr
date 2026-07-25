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

test_that("read_query: a comment marker inside a literal is not a comment", {
  path <- withr::local_tempfile(
    fileext = ".sql",
    lines = c(
      "SELECT '/*' AS a, '*/' AS b, 'it''s' AS c, -- inline comment",
      "  note",
      "FROM patients",
      "WHERE code LIKE '%--%' AND nom != '---' /* block comment */"
    )
  )

  out <- suppressMessages(edstr:::read_query(path))

  expect_no_match(out, "comment")
  expect_match(out, "'%--%'", fixed = TRUE)
  expect_match(out, "'---'", fixed = TRUE)
  expect_match(out, "'/*' AS a, '*/' AS b, 'it''s' AS c", fixed = TRUE)
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

test_that("pua_guard: a missing value does not stop the plane search", {
  expect_length(
    edstr:::.pua_guard(c("texte normal", NA_character_), 2L),
    2L
  )
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

test_that("set_class_css: default-ignorable code points survive the restore", {
  bom <- intToUtf8(0xFEFF)
  pattern <- list(tumeur = "(?i)\\b(cancer)\\b")

  expect_equal(edstr:::set_class_css(bom, pattern), bom)
  expect_equal(
    edstr:::set_class_css(paste0(bom, "cancer"), pattern),
    paste0(bom, "<span class='extract tumeur'>cancer</span>")
  )
})

test_that("easy_ano: hashing replaces the value and keeps a stable width", {
  df <- data.frame(
    ipp = c("111", "222", "111"),
    autre = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  out <- edstr:::easy_ano(df, to_hash = "ipp")

  expect_false(any(out$ipp %in% df$ipp))
  expect_equal(out$autre, df$autre)
  expect_equal(unique(nchar(out$ipp)), 16L)
  expect_equal(out$ipp[[1]], out$ipp[[3]])
  expect_false(out$ipp[[1]] == out$ipp[[2]])
})

test_that("easy_ano: hashing keeps missing values missing", {
  df <- data.frame(
    ipp = c("111", NA, "111", NA),
    num = c(1L, 2L, NA, NA)
  )

  out <- edstr:::easy_ano(df, to_hash = "ipp|num")

  expect_equal(is.na(out$ipp), is.na(df$ipp))
  expect_equal(is.na(out$num), is.na(df$num))
  expect_equal(out$ipp[[1]], out$ipp[[3]])
  expect_equal(unique(nchar(na.omit(out$ipp))), 16L)
})

test_that("easy_ano: hashing a fully missing column leaves it missing", {
  df <- data.frame(ipp = rep(NA_character_, 3L))

  expect_true(all(is.na(edstr:::easy_ano(df, to_hash = "ipp")$ipp)))
})

test_that("easy_ano: hashing is deterministic across calls", {
  df <- data.frame(ipp = "111", stringsAsFactors = FALSE)

  expect_equal(
    edstr:::easy_ano(df, to_hash = "ipp")$ipp,
    edstr:::easy_ano(df, to_hash = "ipp")$ipp
  )
})

test_that("easy_ano: hiding masks every matching column", {
  df <- data.frame(
    nom = c("Dupont", "Martin"),
    prenom = c("Jean", "Marie"),
    autre = c("a", "b"),
    stringsAsFactors = FALSE
  )

  out <- edstr:::easy_ano(df, to_hide = c("nom", "prenom"))

  expect_equal(out$nom, c("---", "---"))
  expect_equal(out$prenom, c("---", "---"))
  expect_equal(out$autre, df$autre)
})

test_that("easy_ano: both patterns are matched case-insensitively", {
  df <- data.frame(
    IPP = c("111", "222"),
    Nom = c("Dupont", "Martin"),
    stringsAsFactors = FALSE
  )

  out <- edstr:::easy_ano(df, to_hash = "ipp", to_hide = "nom")

  expect_false(any(out$IPP %in% df$IPP))
  expect_equal(out$Nom, c("---", "---"))
})

test_that("easy_ano: both patterns take the same regex dialect", {
  df <- data.frame(
    nom_pat = c("Dupont", "Martin"),
    nom_med = c("Durand", "Petit"),
    stringsAsFactors = FALSE
  )

  # a lookahead is the cheapest construct the two engines disagree on
  expect_equal(
    edstr:::easy_ano(df, to_hide = "^nom_(?!med)")$nom_pat,
    c("---", "---")
  )
  expect_equal(
    edstr:::easy_ano(df, to_hide = "^nom_(?!med)")$nom_med,
    df$nom_med
  )
  expect_false(
    any(edstr:::easy_ano(df, to_hash = "^nom_(?!med)")$nom_pat %in% df$nom_pat)
  )
})

test_that("easy_ano: no pattern leaves the frame untouched", {
  df <- data.frame(ipp = "111", stringsAsFactors = FALSE)

  expect_equal(edstr:::easy_ano(df), df)
})

test_that("wb_add_custom: a zero-row sheet keeps a uniform header", {
  empty <- data.frame(a = character(), b = character())

  style <- \(color) {
    wb <- suppressWarnings(
      edstr:::wb_add_custom(openxlsx2::wb_workbook(), "s", empty, color = color)
    )

    unname(c(
      openxlsx2::wb_get_cell_style(wb, "s", dims = "A1"),
      openxlsx2::wb_get_cell_style(wb, "s", dims = "B1")
    ))
  }

  # `wb_dims(select = "data")` collapses onto the header row here, so a
  # data-scoped colour would repaint it
  expect_equal(style(list("#FF0000" = "a")), style(NULL))
  expect_length(unique(style(list("#FF0000" = "a"))), 1L)

  # the `intersect()` normalisation runs only when the frame carries rows
  expect_no_error(style(list("#FF0000" = "absente")))
})

test_that("wb_add_custom: colouring still applies when the frame carries rows", {
  df <- data.frame(a = c("x", "y"), b = c("u", "v"))

  wb <- suppressWarnings(
    edstr:::wb_add_custom(
      openxlsx2::wb_workbook(),
      "s",
      df,
      color = list("#FF0000" = "a")
    )
  )

  cell <- \(dims) unname(openxlsx2::wb_get_cell_style(wb, "s", dims = dims))

  expect_equal(cell("A1"), cell("B1"))
  expect_false(cell("A2") == cell("B2"))
  expect_equal(cell("A2"), cell("A3"))
})
