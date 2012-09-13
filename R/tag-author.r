setClass("AuthorTag", contains = "Tag")
setMethod("writeRd", "AuthorTag", function(object) {
  RdCommand("author", object@text)
})
