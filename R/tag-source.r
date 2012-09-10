setClass("TagSource", contains = "Tag")
setMethod("writeRd", "TagSource", function(object) {
  new_command("source", object@text)
})

