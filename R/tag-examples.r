setClass("ExamplesTag", contains = "Tag")
setMethod("writeRd", "ExamplesTag", function(object) {
  RdCommand("examples", object@text)
})
