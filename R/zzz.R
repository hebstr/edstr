.edstr_java_prev <- NULL

.onLoad <- \(libname, pkgname) {

  if (is.null(getOption("java.parameters"))) {

    .edstr_java_prev <<- getOption("java.parameters")
    options(java.parameters = "-Xmx8g")

  }

}

.onUnload <- \(libpath) {

  options(java.parameters = .edstr_java_prev)

}
