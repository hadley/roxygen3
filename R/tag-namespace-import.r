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
#' @usage @@importFrom package function1 function2
#' @rdname tag-import
#' @autoImports
setClass("TagImportFrom", contains = "Tag")
setMethod("procTag", "TagImportFrom", function(tag) {
  pieces <- str_split(tag@text, "[[:space:]]+")[[1]]
  if (length(pieces) < 2) {
    stop("@importFrom needs at least two components.", call. = FALSE)
  }
  tag@text <- setNames(rep(pieces[1], length(pieces[-1])), pieces[-1])
  tag
})
setMethod("writeNamespace", "TagImportFrom", function(tag) {
  tag <- tag[tag != "base"]
  if (length(tag) == 0) return()
  
  str_c("importFrom(", tag, ",", quote_if_needed(names(tag)), ")", 
    collapse = "\n")
})
setMethod("getPrereqs", "TagImportFrom", function(tag) {
  "TagImportFrom"
})

#' @rdname tag-import
#' @usage @@autoImports
#' @autoImports
setClass("TagAutoImports", contains = "Tag")
setMethod("procBlock", "TagAutoImports", function(tag, block) {
  obj <- block@object
  if (!is.function(obj@value)) return(block)
  
  auto <- auto_imports(obj@value, obj@name, block@tags$importFrom)
  modify_tags(block,
    importFrom = prefix(auto),
    autoImport = NULL)
})

#' @rdname tag-import
#' @usage @@import package1 package2 package3
setClass("TagImport", contains = "Tag")
setMethod("procTag", "TagImport", function(tag) {
  tag@text <- str_split(tag@text, " ")[[1]]
  tag
})
setMethod("writeNamespace", "TagImport", function(tag) {
  ns_each("import")(tag@text)
})

#' @rdname tag-import
#' @usage @@importClassesFrom package fun1 fun2
setClass("TagImportClassesFrom", contains = "Tag")
setMethod("procTag", "TagImportClassesFrom", function(tag) {
  tag@text <- str_split(tag@text, " ")[[1]]
  tag
})
setMethod("writeNamespace", "TagImportClassesFrom", function(tag) {
  ns_repeat1("importClassesFrom")(tag@text)
})

#' @rdname tag-import
#' @usage @@importMethodsFrom package fun1 fun2
setClass("TagImportMethodsFrom", contains = "Tag")
setMethod("procTag", "TagImportMethodsFrom", function(tag) {
  tag@text <- str_split(tag@text, " ")[[1]]
  tag
})
setMethod("writeNamespace", "TagImportMethodsFrom", function(tag) {
  ns_repeat1("importMethodsFrom")(tag@text)
})
