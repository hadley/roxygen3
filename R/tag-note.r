setClass("TagNote", contains = "Tag")
setMethod("writeRd", "TagNote", function(object) {
  new_command("note", object@text)
})
