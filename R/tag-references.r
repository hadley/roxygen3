setClass("TagReferences", contains = "Tag")
setMethod("writeRd", "TagReferences", function(object) {
  new_command("references", object@text)
})
