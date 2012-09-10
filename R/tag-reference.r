setClass("TagReference", contains = "Tag")
setMethod("writeRd", "TagReference", function(object) {
  new_command("reference", object@text)
})
