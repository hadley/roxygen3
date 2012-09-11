setClass("TagFormat", contains = "Tag")

setMethod("defaultTag", c("TagFormat", "DataObject"), 
  function(tag, object) {
    out <- str_c(capture.output(str(object@value, max.level = 1)), 
      collapse = "\n")
    new("TagFormat", text = out)
  }
)

setMethod("writeRd", "TagFormat", function(object) {
  new_command("format", object@text)
})

