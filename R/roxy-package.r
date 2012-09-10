setOldClass("srcref")

#' RoxyPackage class.
#'
#' The package class captures all the information about the package:
#' its name, path, and all the \code{\link{RoxyBlock}}s that it contains.
setClass("RoxyPackage", contains = "RoxyBundle", representation(
  name = "character",
  path = "character"
))

RoxyPackage <- function(path, output = c("writeRd", "writeNamespace", "writeDescription")) {
  pkg <- as.package(path)
  
  load_all(pkg)
  blocks <- in_dir(pkg$path, parse_directory("R", ns_env(pkg)))

  new("RoxyPackage",
    name = pkg$package,
    path = pkg$path, 
    blocks = blocks)
}

setMethod("process", "RoxyPackage", function(input) {
  # Process each block individually for local tags
  input@blocks <- lapply(input@blocks, process)
  
  # # Process the roccers, tags with global behaviour
  # for (roccer in input@roccers) {
  #   input@blocks <- procPackage(roccer, package = input)
  # }

  input
})
