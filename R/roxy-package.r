setOldClass("srcref")

#' RoxyPackage class.
#'
#' The package class captures all the information about the package:
#' its name, path, and all the \code{\link{RoxyBlock}}s that it contains.
setClass("RoxyPackage", contains = "RoxyBundle", representation(
  name = "character",
  path = "character"
))

