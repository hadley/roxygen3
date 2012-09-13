#' Namespace: tags for importing functions.
#'
#' By and large, \code{@@autoImports} should be the only imports
#' tag that you need to use. It automatically generates the necessary
#' \code{importFrom} statements to import all external functions used by this
#' function.  See \code{\link{auto_imports}} for more implementation details.
#'
#' If there is a conflict, use \code{tag_importFrom} to resolve it. You can do
#' \code{@@importFrom base function} - this is not normally allowed in the
#' \file{NAMESPACE}, but roxygen3 will simply ignore it, but still use it when
#'  resolving conflicts.
#'
#' You must have the packages declared in \code{DESCRIPTION} Imports.
#'
#'
#' @usageTag @@importFrom package function1 function2
#' @rdname tag-import
#' @autoImports
setClass("ImportFromTag", contains = "Tag", representation(
  imports = "character"))

setMethod("value", "ImportFromTag", function(tag) tag@imports)

setMethod("value<-", "ImportFromTag", function(tag, value) {
  if (length(value) == 0) return(tag)

  pieces <- str_split(value, "[[:space:]]+")[[1]]
  if (length(pieces) < 2) {
    stop("@importFrom needs at least two components.", call. = FALSE)
  }
  tag@imports <- c(tag@imports,
    setNames(rep(pieces[1], length(pieces[-1])), pieces[-1]))
  tag
})
setMethod("writeNamespace", "ImportFromTag", function(object) {
  imports <- object@imports

  imports <- imports[imports != "base"]
  if (length(imports) == 0) return()

  str_c("importFrom(", imports, ",", quote_if_needed(names(imports)), ")")
})
