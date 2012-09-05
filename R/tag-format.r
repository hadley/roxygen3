setClass("TagFormat", contains = "Tag")
setMethod("writeRd", "TagFormat", function(tag) {
  new_command("format", tag@text)
})
