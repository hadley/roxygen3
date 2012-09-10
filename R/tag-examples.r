setClass("TagExamples", contains = "Tag")
setMethod("writeRd", "TagExamples", function(object) {
  new_command("examples", object@text)
})
