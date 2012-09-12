setClass("RoxyDir", contains = "Bundle", representation(
  path = "character")
)

RoxyDir <- function(path, behaviour = default_behaviour()) {
  blocks <- in_dir(path, parse_directory("."))

  new("RoxyDir",
    path = path,
    blocks = blocks,
    behaviour = behaviour)
}


#' RoxyPackage class.
#'
#' The package class captures all the information about the package:
#' its name, path, and all the \code{\link{Block}}s that it contains.
setClass("RoxyPackage", contains = "RoxyDir", representation(
  name = "character"))

#' @autoImports
RoxyPackage <- function(path, behaviour = default_behaviour()) {
  pkg <- as.package(path)
  
  load_all(pkg)
  blocks <- in_dir(pkg$path, {
    parse_directory("R", ns_env(pkg), tags = behaviour@tags)
  })

  new("RoxyPackage",
    name = pkg$package,
    path = pkg$path,
    blocks = blocks,
    behaviour = behaviour)
}


setGeneric("rPath", function(bundle) {
  standardGeneric("rPath")
})
setMethod("rPath", "RoxyPackage", function(bundle) { 
  file.path(bundle@path, "R")
})
setMethod("rPath", "RoxyDir", function(bundle) { 
  bundle@path
})
setMethod("rPath", "Bundle", function(bundle) { 
  NULL
})
