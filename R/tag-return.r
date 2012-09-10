setClass("TagReturn", contains = "Tag")

setMethod("writeRd", "TagReturn", function(object) {
  new_command("value", object@text)
})
