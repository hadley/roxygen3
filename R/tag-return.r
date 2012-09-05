setClass("TagReturn", contains = "Tag")

setMethod("writeRd", "TagReturn", function(tag) {
  new_command("value", tag@text)
})
