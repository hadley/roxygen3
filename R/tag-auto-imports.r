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
