#' PackageBundle class.
#'
#' The package class captures all the information about the package:
#' its name, path, and all the \code{\link{Block}}s that it contains.

#' @autoImports
PackageBundle <- function(path, behaviour = default_behaviour()) {
  pkg <- as.package(path)
  
  load_all(pkg)
  blocks <- in_dir(pkg$path, {
    parse_directory("R", ns_env(pkg), tags = behaviour@tags)
  })

  new("PackageBundle",
    name = pkg$package,
    path = pkg$path,
    blocks = blocks,
    behaviour = behaviour)
}


setGeneric("rPath", function(bundle) {
  standardGeneric("rPath")
})
setMethod("rPath", "PackageBundle", function(bundle) { 
  file.path(bundle@path, "R")
})
setMethod("rPath", "DirectoryBundle", function(bundle) { 
  bundle@path
})
setMethod("rPath", "Bundle", function(bundle) { 
  NULL
})
