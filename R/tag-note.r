setClass("TagNote", contains = "Tag")
setMethod("writeRd", "TagNote", function(tag) {
  new_command("note", tag@text)
})
