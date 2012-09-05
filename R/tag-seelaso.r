setClass("TagSeealso", contains = "Tag")
setMethod("writeRd", "TagSeealso", function(tag) {
  new_command("seealso", tag@text)
})

