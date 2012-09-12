#' A bundle of files in a directory.
#'
#' @param path location of folder containing R files
#' @param behaviour \code{\link{Behaviour}} object describing what tags,
#'   processors and writers to use
#' @export
#' @dev
DirectoryBundle <- function(path, behaviour = default_behaviour()) {
  r_files <- dir(path, pattern = "\\.[RrSs]$", full.names = TRUE)

  env <- new.env(parent = globalenv())
  lapply(r_files, sys.source, envir = env, chdir = TRUE)

  blocks <- in_dir(path, {
    parse_directory(".", env, tags = behaviour@tags)
  })

  new("DirectoryBundle",
    path = path,
    blocks = blocks,
    behaviour = behaviour)
}

