setClass("TagSeealso", contains = "Tag")
setMethod("writeRd", "TagSeealso", function(object) {
  new_command("seealso", object@text)
})

