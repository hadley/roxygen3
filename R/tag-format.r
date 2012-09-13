setClass("FormatTag", contains = "Tag")

setMethod("defaultTag", c("FormatTag", "DataObject"),
  function(tag, object) {
    out <- str_c(capture.output(str(object@value, max.level = 1)),
      collapse = "\n")
    new("FormatTag", text = out)
  }
)

setMethod("writeRd", "FormatTag", function(object) {
  RdCommand("format", object@text)
})

