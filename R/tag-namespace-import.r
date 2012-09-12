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
#' @tagUsage @@importFrom package function1 function2
#' @rdname tag-import
#' @autoImports
setClass("TagImportFrom", contains = "Tag", representation(
  imports = "character"))
setMethod("procTag", "TagImportFrom", function(tag) {
  if (length(tag@text) == 0) return(tag)

  pieces <- str_split(tag@text, "[[:space:]]+")[[1]]
  if (length(pieces) < 2) {
    stop("@importFrom needs at least two components.", call. = FALSE)
  }
  tag@imports <- c(tag@imports,
    setNames(rep(pieces[1], length(pieces[-1])), pieces[-1]))
  tag
})
setMethod("writeNamespace", "TagImportFrom", function(object) {
  imports <- object@imports

  imports <- imports[imports != "base"]
  if (length(imports) == 0) return()

  str_c("importFrom(", imports, ",", quote_if_needed(names(imports)), ")")
})

#' @rdname tag-import
#' @tagUsage @@autoImports
#' @autoImports
setClass("TagAutoImports", contains = "Tag")
setMethod("procBlock", "TagAutoImports", function(tag, block) {
  obj <- block@object
  if (!is.function(obj@value)) return(block)

  importFrom <- block@tags$importFrom
  auto <- auto_imports(obj@value, obj@name, importFrom)

  if (!is.null(importFrom)) {
    importFrom@imports <- c(importFrom@imports, auto)
  } else {
    importFrom <- new("TagImportFrom", imports = auto)
  }

  tag(block, "importFrom") <- importFrom
  tag(block, "autoImport") <- NULL
  block
})

setMethod("getPrereqs", "TagAutoImports", function(tag) {
  "TagImportFrom"
})

#' @rdname tag-import
#' @tagUsage @@import package1 package2 package3
setClass("TagImport", contains = "Tag")
setMethod("procTag", "TagImport", function(tag) {
  tag@text <- str_split(tag@text, " ")[[1]]
  tag
})
setMethod("writeNamespace", "TagImport", function(object) {
  ns_each("import", object@text)
})

#' @rdname tag-import
#' @tagUsage @@importClassesFrom package fun1 fun2
setClass("TagImportClassesFrom", contains = "Tag")
setMethod("procTag", "TagImportClassesFrom", function(tag) {
  tag@text <- str_split(tag@text, " ")[[1]]
  tag
})
setMethod("writeNamespace", "TagImportClassesFrom", function(object) {
  ns_repeat1("importClassesFrom",object@text)
})

#' @rdname tag-import
#' @tagUsage @@importMethodsFrom package fun1 fun2
setClass("TagImportMethodsFrom", contains = "Tag")
setMethod("procTag", "TagImportMethodsFrom", function(tag) {
  tag@text <- str_split(tag@text, " ")[[1]]
  tag
})
setMethod("writeNamespace", "TagImportMethodsFrom", function(object) {
  ns_repeat1("importMethodsFrom",object@text)
})
