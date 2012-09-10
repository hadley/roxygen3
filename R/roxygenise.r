#' @param roccer a list of roccers that you want to use.  By default these
#'   will just be the roccers provided by roxygen3.
#' @param path path to package
#' @autoImports
#' @export
roxygenise <- function(path, tags = base_roccers(), check = FALSE, clean = FALSE) {
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

# Roccers need to be run in a specific order and later roccers need to be
# able to see results of earlier roccers.
roxy_process <- function(blocks, roccers = list()) {
  blocks <- lapply(blocks, roxyProcess)
  
  for (roccer in roccers) {
    blocks <- roccer(blocks)
  }

  blocks
}

# Rocouts can be run in any order and are completely independent.
roxy_out <- function(rocblocks, roccers = base_roccers()) {
  out <- list()
  for (roccer in roccers) {
    rocout <- roccer$output
    if (is.null(rocout)) next
    
    type <- class(rocout)[1]
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

# Post-processing of output objects before they're written to disk
roxy_postproc <- function(output) {
  out <- list()
  for (writer in names(output)) {
    fname <- str_c("output_postproc.", writer)
    f <- find_fun(fname)
    if (is.null(f)) stop("Can't find method ", fname)
    out[[writer]] <- lapply(output[[writer]], f)
  }
  out
}

roxy_write <- function(out, out_path) {
  writers <- names(out)
  for (writer in writers) {
    fname <- str_c("output_write.", writer)
    f <- find_fun(fname)
    if (is.null(f)) stop("Can't find method ", fname)
    
    for(path in names(out[[writer]])) {
      f(out[[writer]][[path]], file.path(out_path, path))
    }
  }
  invisible()
}

subclassnames <- function(class) {
  names(getClass(class)@subclasses)
}
