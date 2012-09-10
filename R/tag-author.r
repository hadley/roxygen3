setClass("TagAuthor", contains = "Tag")
setMethod("writeRd", "TagAuthor", function(object) {
  new_command("author", object@text)
})
