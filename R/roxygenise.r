#' @param roccer a list of roccers that you want to use.  By default these
#'   will just be the roccers provided by roxygen3.
#' @param path path to package
#' @autoImports
#' @export
roxygenise <- function(path, check = FALSE, clean = FALSE) {
  pkg <- RoxyPackage(path)
  
  man_path <- file.path(pkg@path, "man")
  if (clean && file.exists(man_path)) {
    rd <- dir(man_path, pattern = "\\.Rd$", full.names = TRUE)
    file.remove(rd)
  }
  
  pkg <- process(pkg)
  write(pkg)
  
  if (check) {
    check_doc(pkg)
  }
  
  invisible(rocblocks)
}
