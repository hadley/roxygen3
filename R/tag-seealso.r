setClass("SeealsoTag", contains = "Tag")
setMethod("writeRd", "SeealsoTag", function(object) {
  RdCommand("seealso", object@text)
})
