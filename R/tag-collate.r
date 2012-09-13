setClass("TagCollate", contains = "Tag",
  list(files = "character"))

setMethod("format", "TagCollate", function(x, ...) x@files %||% x@text)

setMethod("writeDescription", "TagCollate", function(object) {
  list(collate = object@files)
})
