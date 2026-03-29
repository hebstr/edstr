.edstr_java_prev <- NULL
.edstr_java_changed <- FALSE

.onLoad <- \(libname, pkgname) {

  .edstr_java_prev <<- getOption("java.parameters")

  if (is.null(.edstr_java_prev)) {
    options(java.parameters = "-Xmx8g")
    .edstr_java_changed <<- TRUE
  }

}

.onUnload <- \(libpath) {

  if (.edstr_java_changed) {
    options(java.parameters = .edstr_java_prev)
  }

}
