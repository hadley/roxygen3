setClass("CollateTag", contains = "Tag",
  list(files = "character"))

setMethod("format", "CollateTag", function(x, ...) x@files %||% x@text)

setMethod("writeDescription", "CollateTag", function(object) {
  list(collate = object@files)
})
