setClass("TagReference", contains = "Tag")
setMethod("writeRd", "TagReference", function(tag) {
  new_command("reference", tag@text)
})
