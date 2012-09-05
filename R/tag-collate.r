setClass("TagCollate", contains = "Tag",
  list(files = "character"))

setMethod("writeDescription", "TagCollate", function(tag) {
  list(collate = tag@files)
})
