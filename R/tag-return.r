setClass("ReturnTag", contains = "Tag")

setMethod("writeRd", "ReturnTag", function(object) {
  RdCommand("value", object@text)
})
