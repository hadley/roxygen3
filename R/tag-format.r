setClass("TagFormat", contains = "Tag")

setMethod("defaultTag", c("TagFormat", "DataObject"), 
  function(tag, object) {
    str_c(capture.output(str(object@value, max.level = 1)), collapse = "\n")
  }
)

setMethod("writeRd", "TagFormat", function(object) {
  new_command("format", object@text)
})

