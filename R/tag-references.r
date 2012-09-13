setClass("ReferencesTag", contains = "Tag")
setMethod("writeRd", "ReferencesTag", function(object) {
  RdCommand("references", object@text)
})
