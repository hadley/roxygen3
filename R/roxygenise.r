#' @param roccer a list of roccers that you want to use.  By default these
#'   will just be the roccers provided by roxygen3.
#' @param path path to package
#' @auto_imports
roxygenise <- function(path, roccers = base_roccers()) {
  pkg <- as.package(path)
  
  load_all(pkg)
  in_dir(pkg$path, {
    rocblocks <- parse_directory("R", ns_env(pkg))

    # Roccers need to be run in a specific order and later roccers need to be
    # able to see results of earlier roccers.
    for (roccer in roccers) {
      rocblocks <- parse_rocblocks(roccer$parser, rocblocks)
    }

    # Rocouts can be run in any order and are completely independent.
    write_out(roccers, rocblocks, ".")
    
  })
}
