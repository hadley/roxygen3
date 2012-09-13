setClass("AuthorTag", contains = "Tag")
setMethod("writeRd", "AuthorTag", function(object) {
  new_command("author", object@text)
})
