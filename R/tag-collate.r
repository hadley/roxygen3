setClass("TagCollate", contains = "Tag",
  list(files = "character"))

setMethod("writeDescription", "TagCollate", function(object) {
  list(collate = object@files)
})
