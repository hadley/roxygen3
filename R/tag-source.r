setClass("TagSource", contains = "Tag")
setMethod("writeRd", "TagSource", function(tag) {
  new_command("source", tag@text)
})

