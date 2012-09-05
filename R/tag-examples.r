setClass("TagExamples", contains = "Tag")
setMethod("writeRd", "TagExamples", function(tag) {
  new_command("examples")
})
