setClass("TagAuthor", contains = "Tag")
setMethod("writeRd", "TagAuthor", function(tag) {
  new_command("author", tag@text)
})
