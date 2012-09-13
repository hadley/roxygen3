setClass("ReferencesTag", contains = "Tag")
setMethod("writeRd", "ReferencesTag", function(object) {
  new_command("references", object@text)
})
