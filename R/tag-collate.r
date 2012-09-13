setClass("CollateTag", contains = "Tag",
  list(files = "character"))

setMethod("value", "CollateTag", function(tag) {
  tag@files
})
setMethod("value<-", "CollateTag", function(tag, value) {
  tag@files <- value
  tag
})

setMethod("writeDescription", "CollateTag", function(object) {
  list(collate = object@files)
})
