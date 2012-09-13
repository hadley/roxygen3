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

#' @rdname tag-import
#' @usageTag @@autoImports
#' @autoImports
setClass("AutoImportsTag", contains = "Tag")
setMethod("process", "AutoImportsTag", function(input, block) {
  obj <- block@object
  tag(block, "autoImport") <- NULL
  if (!is.function(obj@value)) return(block)

  importFrom <- tag(block, "importFrom")
  auto <- auto_imports(obj@value, obj@name, value(importFrom))
  importFrom@imports <- c(importFrom@imports, auto)

  tag(block, "importFrom") <- importFrom
  block
})

setMethod("getPrereqs", "AutoImportsTag", function(tag) {
  "ImportFromTag"
})

#' @rdname tag-import
#' @usageTag @@import package1 package2 package3
setClass("ImportTag", contains = "Tag")
setMethod("value<-", "ImportTag", function(tag, value) {
  tag@text <- str_split(value, " ")[[1]]
  tag
})
setMethod("writeNamespace", "ImportTag", function(object) {
  ns_each("import", object@text)
})

#' @rdname tag-import
#' @usageTag @@importClassesFrom package fun1 fun2
setClass("ImportClassesFromTag", contains = "Tag")
setMethod("value<-", "ImportClassesFromTag", function(tag, value) {
  tag@text <- str_split(value, " ")[[1]]
  tag
})
setMethod("writeNamespace", "ImportClassesFromTag", function(object) {
  ns_repeat1("importClassesFrom",object@text)
})

#' @rdname tag-import
#' @usageTag @@importMethodsFrom package fun1 fun2
setClass("ImportMethodsFromTag", contains = "Tag")
setMethod("value<-", "ImportMethodsFromTag", function(tag, value) {
  tag@text <- str_split(value, " ")[[1]]
  tag
})
setMethod("writeNamespace", "ImportMethodsFromTag", function(object) {
  ns_repeat1("importMethodsFrom",object@text)
})
