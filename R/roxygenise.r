#' @param roccer a list of roccers that you want to use.  By default these
#'   will just be the roccers provided by 
roxygenise <- function(path, env = NULL, roccers = base_roccers()) {

  rocblocks <- parse_directory(path, env)
  
  # Roccers need to be run in a specific order and later roccers need to be
  # able to see results of earlier roccers.
  for (roccer in roccers) {
    rocblocks <- parse_rocblocks(roccer$parser, rocblocks)
  }

  # Rocouts can be run in any order and are completely independent.
}
