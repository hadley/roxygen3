#' Roxygenise a package.
#'
#' This converts roxygen comments in to Rd files in the \file{man/} directory,
#' directives in the \file{NAMESPACE}, and fields in the \file{DESCRIPTION}.
#'
#' @param path path to package
#' @param check should \code{\link[devtools]{check_doc}} be run after
#'   documentation is complete?
#' @param clean if \code{TRUE} will delete the contents of \file{man/} before
#'   generating output.
#' @autoImports
#' @export
roxygenise <- function(path, check = FALSE, clean = FALSE) {
  pkg <- PackageBundle(path)

  man_path <- file.path(pkg@path, "man")
  if (clean && file.exists(man_path)) {
    rd <- dir(man_path, pattern = "\\.Rd$", full.names = TRUE)
    file.remove(rd)
  }

  process(pkg)

  if (check) {
    check_doc(pkg)
  }

  invisible(pkg)
}
