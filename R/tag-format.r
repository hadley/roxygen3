setClass("TagFormat", contains = "Tag")
setMethod("writeRd", "TagFormat", function(object) {
  new_command("format", object@text)
})
