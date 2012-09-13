setClass("SeealsoTag", contains = "Tag")
setMethod("writeRd", "SeealsoTag", function(object) {
  new_command("seealso", object@text)
})
