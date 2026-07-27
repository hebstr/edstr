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
      data_id = data.frame(
        doc_id = "1",
        concept = "lesion",
        texte = "dysplasie",
        stringsAsFactors = FALSE
      ),
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

# applied in sequence, a rule on a character the built-ins emit rewrites their
# output: `s` lands inside the `[\s\-']` separator and every source match dies
test_that("match_source: a regex_replace rule cannot rewrite the built-in machinery", {
  data_match_df <- data.frame(
    doc_id = "1",
    id_group = 1,
    texte = "accident vasculaire cerebral",
    stringsAsFactors = FALSE
  )

  data_count <- data.frame(
    concept = "avc",
    texte = "accident vasculaire",
    stringsAsFactors = FALSE
  )

  match_source <- \(regex_replace) {
    edstr:::.extract_match_source(
      data_match_df = data_match_df,
      data_count = data_count,
      data_id = data.frame(
        doc_id = "1",
        concept = "avc",
        texte = "accident vasculaire",
        stringsAsFactors = FALSE
      ),
      text_input = "texte",
      id = "doc_id",
      regex_replace = regex_replace
    )
  }

  expect_equal(
    match_source(c("s" = "[sz]"))$data_regex_match$match,
    "accident vasculaire"
  )

  expect_equal(
    match_source(c("a" = "[a4]"))$data_regex_match$match,
    "accident vasculaire"
  )
})

# a trigram and its constituent bigram both match at the same position, and the
# bigram is always the more frequent of the two, so it is what `count(sort =
# TRUE)` puts first. Ordering by length is what keeps the trigram from losing
# every position to it.
ngram_source <- \(
  tokens = "accident vasculaire cerebral",
  texte = "un accident vasculaire cerebral massif"
) {
  edstr:::.extract_match_source(
    data_match_df = data.frame(
      doc_id = "1",
      id_group = 1,
      texte = texte,
      stringsAsFactors = FALSE
    ),
    data_count = data.frame(
      concept = "avc",
      texte = c("accident vasculaire", "cerebral", "accident vasculaire cerebral"),
      n = c(12L, 9L, 3L),
      stringsAsFactors = FALSE
    ),
    data_id = data.frame(
      doc_id = "1",
      concept = "avc",
      texte = tokens,
      stringsAsFactors = FALSE
    ),
    text_input = "texte",
    id = "doc_id",
    regex_replace = NULL
  )
}

test_that("match_source: alternation branches are ordered by decreasing length", {
  branches <- strsplit(ngram_source()$data_regex_list$avc, "|", fixed = TRUE)[[1]]

  expect_length(branches, 3)
  expect_equal(nchar(branches), sort(nchar(branches), decreasing = TRUE))
})

test_that("match_source: a trigram wins the position over its bigram", {
  expect_equal(
    ngram_source()$data_regex_match$match,
    "accident vasculaire cerebral"
  )
})

# re2 runs in longest-match mode, so it would merge the trigram whatever the
# branch order: only a leftmost-first engine tells a correct sort from a broken
# one, and that is what `set_class_css()` and the ICU fallback run on
test_that("match_source: branch order alone merges the trigram, leftmost-first", {
  text <- "un accident vasculaire cerebral massif"
  pattern <- as.character(ngram_source()$data_regex_str)

  expect_equal(
    regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]],
    "accident vasculaire cerebral"
  )
})

# the alternation returns one match per position, so the n-grams the winning
# branch displaced have to be recovered by re-testing each one on its own
test_that("match_source: a displaced n-gram is repaired, keeping the winner", {
  out <- ngram_source(
    tokens = c("accident vasculaire cerebral", "accident vasculaire", "cerebral")
  )

  expect_setequal(
    out$data_regex_match$match,
    c("accident vasculaire cerebral", "accident vasculaire", "cerebral")
  )
  expect_equal(nrow(out$data_regex_count), 3)
})

# a `regex_replace` the user supplies is invisible to the token-to-span key, so
# a token confirmed only through one reads as unconfirmed here; deciding the
# addition on the span keeps the pass from recording that match twice
test_that("match_source: repair does not duplicate a match confirmed through regex_replace", {
  out <- edstr:::.extract_match_source(
    data_match_df = data.frame(
      doc_id = "1",
      texte = "displasie moderee",
      stringsAsFactors = FALSE
    ),
    data_count = data.frame(
      concept = "lesion",
      texte = "dysplasie",
      stringsAsFactors = FALSE
    ),
    data_id = data.frame(
      doc_id = "1",
      concept = "lesion",
      texte = "dysplasie",
      stringsAsFactors = FALSE
    ),
    text_input = "texte",
    id = "doc_id",
    regex_replace = c("y" = "[yi]")
  )

  expect_equal(out$data_regex_match$match, "displasie")
})

# two unconfirmed tokens can reach the same span, each finding all of its
# occurrences, so summing the groups records it once per token that reaches it
test_that("match_source: repair counts a span once when two tokens reach it", {
  tokens <- c("dysplasie", "displasie", "dysplasie moderee", "displasie severe")

  out <- edstr:::.extract_match_source(
    data_match_df = data.frame(
      doc_id = "1",
      texte = "dysplasie moderee et displasie severe",
      stringsAsFactors = FALSE
    ),
    data_count = data.frame(
      concept = "lesion",
      texte = tokens,
      stringsAsFactors = FALSE
    ),
    data_id = data.frame(
      doc_id = "1",
      concept = "lesion",
      texte = tokens,
      stringsAsFactors = FALSE
    ),
    text_input = "texte",
    id = "doc_id",
    regex_replace = c("y" = "[yi]")
  )

  expect_equal(
    sum(out$data_regex_match$match == "displasie"),
    1
  )
})

test_that("match_source: repair recovers the original surface form, not the token", {
  out <- ngram_source(
    tokens = c("accident vasculaire cerebral", "accident vasculaire"),
    texte = "un Accident Vasculaire Cérébral massif"
  )

  expect_true("Accident Vasculaire" %in% out$data_regex_match$match)
})

test_that("match_source: repair adds one row per occurrence", {
  out <- ngram_source(
    tokens = c("accident vasculaire cerebral", "accident vasculaire"),
    texte = "accident vasculaire cerebral puis accident vasculaire cerebral"
  )

  expect_equal(sum(out$data_regex_match$match == "accident vasculaire"), 2)
})

# the key is an existence test, so a token confirmed where it stands alone loses
# the occurrence a longer branch took: both have to be recorded
test_that("match_source: repair recovers a token confirmed at another position", {
  out <- ngram_source(
    tokens = c("accident vasculaire cerebral", "accident vasculaire"),
    texte = "accident vasculaire cerebral et accident vasculaire seul"
  )

  expect_equal(sum(out$data_regex_match$match == "accident vasculaire"), 2)
})

test_that("match_source: a token absent from the source stays unrepaired", {
  out <- ngram_source(
    tokens = c("accident vasculaire cerebral", "thrombose"),
    texte = "un accident vasculaire cerebral massif"
  )

  expect_equal(out$data_regex_match$match, "accident vasculaire cerebral")
})

test_that("match_source: the highlight covers the whole trigram, not its head", {
  css <- edstr:::set_class_css(
    "un accident vasculaire cerebral massif",
    ngram_source()$data_regex_list
  )

  # the head must not carry a span of its own: a broken sort splits the trigram
  # into two adjacent spans rather than leaving its tail bare, so asserting on
  # the unhighlighted tail would hold under both orders
  expect_match(css, ">accident vasculaire cerebral<", fixed = TRUE)
  expect_no_match(css, ">accident vasculaire<", fixed = TRUE)
})
