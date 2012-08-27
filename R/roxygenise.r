#' @param roccer a list of roccers that you want to use.  By default these
#'   will just be the roccers provided by roxygen3.
#' @param path path to package
#' @auto_imports
roxygenise <- function(path, roccers = base_roccers(), check = FALSE, clean = FALSE) {
  pkg <- as.package(path)
  
  man_path <- file.path(path, "man")
  if (clean && file.exists(man_path)) {
    rd <- dir(man_path, pattern = "\\.Rd$", full.names = TRUE)
    file.remove(rd)
  }
  
  load_all(pkg)
  in_dir(pkg$path, {
    rocblocks <- parse_directory("R", ns_env(pkg))
    rocblocks <- roxy_process(rocblocks, roccers)
    
    out <- roxy_out(rocblocks, roccers)
    roxy_write(out, ".")
  })
  
  if (check) {
    check_doc(pkg)
  }
  
  invisible(rocblocks)
}

# Roccers need to be run in a specific order and later roccers need to be
# able to see results of earlier roccers.
roxy_process <- function(rocblocks, roccers = base_roccers()) {
  for (roccer in roccers) {
    rocblocks <- parse_rocblocks(roccer$parser, rocblocks)
  }
  rocblocks
}

# Rocouts can be run in any order and are completely independent.
roxy_out <- function(rocblocks, roccers = base_roccers()) {
  out <- list()
  for (roccer in roccers) {
    rocout <- roccer$output
    if (is.null(rocout)) next
    
    type <- output_type(rocout)
    if (is.null(out[[type]])) out[[type]] <- list()
    
    for (rocblock in rocblocks) {
      tag <- rocblock$roc[[rocout$name]]
      if (is.null(tag)) next
      
      path <- output_path(rocout, rocblock)
      if (is.null(path)) next
      n <- length(out[[type]][[path]]) + 1
      if (n == 1) out[[type]][[path]] <- list()
      out[[type]][[path]][n] <- rocout$tag(tag)
    }
  }
  out
}
roxy_write <- function(out, out_path) {
  writers <- names(out)
  for (writer in writers) {
    match.fun(writer)(out[[writer]], out_path)
  }
}
