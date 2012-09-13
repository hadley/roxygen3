setClass("SourceTag", contains = "Tag")
setMethod("writeRd", "SourceTag", function(object) {
  new_command("source", object@text)
})

