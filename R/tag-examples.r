setClass("ExamplesTag", contains = "Tag")
setMethod("writeRd", "ExamplesTag", function(object) {
  new_command("examples", object@text)
})
