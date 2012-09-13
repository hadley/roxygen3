setClass("ReturnTag", contains = "Tag")

setMethod("writeRd", "ReturnTag", function(object) {
  new_command("value", object@text)
})
