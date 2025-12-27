# Title

Title

## Usage

``` r
edstr_extract(
  data,
  text_input = getOption("edstr_text"),
  sample = NULL,
  seed = NULL,
  ano_hash = NULL,
  ano_hide = NULL,
  id = "",
  group = "",
  token = 1,
  concepts,
  collapse = FALSE,
  intersect = FALSE,
  starts_with_only = TRUE,
  exclus_manual = NULL,
  exclus_auto_escape = NULL,
  regex_replace = NULL,
  mismatch_data = FALSE,
  concept_color = "#0099FF",
  text_color = "#FF0000",
  text_background = "#FFFF00",
  dirname_suffix = if (!is.null(sample)) glue("sample_{sample}") else NULL,
  filename_suffix = dirname_suffix,
  load = FALSE,
  quiet = FALSE
)
```

## Arguments

- data:

  data

- text_input:

  text_input

- sample:

  sample

- seed:

  seed

- ano_hash:

  ano_hash

- ano_hide:

  ano_hide

- id:

  id

- group:

  group

- token:

  token

- concepts:

  concepts

- collapse:

  collapse

- intersect:

  intersect

- starts_with_only:

  starts_with_only

- exclus_manual:

  exclus_manual

- exclus_auto_escape:

  exclus_auto_escape

- regex_replace:

  regex_replace

- mismatch_data:

  mismatch_data

- concept_color:

  concept_color

- text_color:

  text_color

- text_background:

  text_background

- dirname_suffix:

  dirname_suffix

- filename_suffix:

  filename_suffix

- load:

  load

- quiet:

  quiet

## Value

value

## Examples

``` r
example
#> function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, 
#>     give.lines = FALSE, local = FALSE, type = c("console", "html"), 
#>     echo = TRUE, verbose = getOption("verbose"), setRNG = FALSE, 
#>     ask = getOption("example.ask"), prompt.prefix = abbreviate(topic, 
#>         6), catch.aborts = FALSE, run.dontrun = FALSE, run.donttest = interactive()) 
#> {
#>     type <- match.arg(type)
#>     html <- type == "html"
#>     if (html) {
#>         enhancedHTML <- str2logical(Sys.getenv("_R_HELP_ENABLE_ENHANCED_HTML_", 
#>             "TRUE"))
#>         if (!interactive() || !enhancedHTML || !requireNamespace("knitr", 
#>             quietly = TRUE)) 
#>             html <- FALSE
#>     }
#>     if (html) {
#>         port <- tools::startDynamicHelp(NA)
#>         if (port <= 0L) 
#>             html <- FALSE
#>         else {
#>             if (!is.null(lib.loc)) 
#>                 lib.loc <- NULL
#>             browser <- if (.Platform$GUI == "AQUA") {
#>                 get("aqua.browser", envir = as.environment("tools:RGUI"))
#>             }
#>             else getOption("browser")
#>         }
#>     }
#>     if (!character.only) {
#>         topic <- substitute(topic)
#>         if (!is.character(topic)) 
#>             topic <- deparse(topic)[1L]
#>     }
#>     pkgpaths <- find.package(package, lib.loc, verbose = verbose)
#>     file <- index.search(topic, pkgpaths, firstOnly = TRUE)
#>     if (!length(file)) {
#>         warning(gettextf("no help found for %s", sQuote(topic)), 
#>             domain = NA)
#>         return(invisible())
#>     }
#>     if (verbose) 
#>         cat("Found file =", sQuote(file), "\n")
#>     packagePath <- dirname(dirname(file))
#>     pkgname <- basename(packagePath)
#>     if (html) {
#>         query <- if (local) 
#>             ""
#>         else "?local=FALSE"
#>         browseURL(paste0("http://127.0.0.1:", port, "/library/", 
#>             pkgname, "/Example/", topic, query), browser)
#>         return(invisible())
#>     }
#>     lib <- dirname(packagePath)
#>     tf <- tempfile("Rex")
#>     tools::Rd2ex(.getHelpFile(file), tf, commentDontrun = !run.dontrun, 
#>         commentDonttest = !run.donttest)
#>     if (!file.exists(tf)) {
#>         if (give.lines) 
#>             return(character())
#>         warning(gettextf("%s has a help file but no examples", 
#>             sQuote(topic)), domain = NA)
#>         return(invisible())
#>     }
#>     on.exit(unlink(tf))
#>     if (give.lines) 
#>         return(readLines(tf))
#>     if (pkgname != "base") 
#>         library(pkgname, lib.loc = lib, character.only = TRUE)
#>     if (!is.logical(setRNG) || setRNG) {
#>         if ((exists(".Random.seed", envir = .GlobalEnv))) {
#>             oldSeed <- get(".Random.seed", envir = .GlobalEnv)
#>             on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv), 
#>                 add = TRUE)
#>         }
#>         else {
#>             oldRNG <- RNGkind()
#>             on.exit(RNGkind(oldRNG[1L], oldRNG[2L], oldRNG[3L]), 
#>                 add = TRUE)
#>         }
#>         if (is.logical(setRNG)) {
#>             RNGkind("default", "default", "default")
#>             set.seed(1)
#>         }
#>         else eval(setRNG)
#>     }
#>     zz <- readLines(tf, n = 1L)
#>     skips <- 0L
#>     if (echo) {
#>         zcon <- file(tf, open = "rt")
#>         while (length(zz) && !length(grep("^### \\*\\*", zz))) {
#>             skips <- skips + 1L
#>             zz <- readLines(zcon, n = 1L)
#>         }
#>         close(zcon)
#>     }
#>     if (ask == "default") 
#>         ask <- echo && grDevices::dev.interactive(orNone = TRUE)
#>     if (ask) {
#>         oldask <- if (.Device != "null device") 
#>             grDevices::devAskNewPage(ask = TRUE)
#>         else getOption("device.ask.default", FALSE)
#>         on.exit(if (.Device != "null device") grDevices::devAskNewPage(oldask), 
#>             add = TRUE)
#>         op <- options(device.ask.default = TRUE)
#>         on.exit(options(op), add = TRUE)
#>     }
#>     source(tf, local, echo = echo, prompt.echo = paste0(prompt.prefix, 
#>         getOption("prompt")), continue.echo = paste0(prompt.prefix, 
#>         getOption("continue")), verbose = verbose, max.deparse.length = Inf, 
#>         encoding = "UTF-8", catch.aborts = catch.aborts, skip.echo = skips, 
#>         keep.source = TRUE)
#> }
#> <bytecode: 0x638a857d6648>
#> <environment: namespace:utils>
```
