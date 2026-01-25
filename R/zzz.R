.onLoad <- \(libname, pkgname) {

  if (is.null(getOption("java.parameters"))) {

    options(java.parameters = "-Xmx8g")

  }

}
