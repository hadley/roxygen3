setOldClass("srcref")

#' RoxyPackage class.
#'
#' The package class captures all the information about the package:
#' its name, path, and all the \code{\link{RoxyBlock}}s that it contains.
setClass("RoxyPackage", contains = "RoxyBundle", representation(
  name = "character",
  path = "character"
))

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
