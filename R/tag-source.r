setClass("SourceTag", contains = "Tag")
setMethod("writeRd", "SourceTag", function(object) {
  RdCommand("source", object@text)
})

