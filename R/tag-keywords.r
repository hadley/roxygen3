setClass("TagKeywords", contains = "Tag")

setMethod("defaultTag", c("TagKeywords", "DataObject"), function(tag, object) {
  new("TagKeywords", text = "datasets")
})

setMethod("procTag", "TagKeywords", function(tag) {
  parse_words(tag)
})

setMethod("writeRd", "TagKeywords", function(object) {
  new_command("keyword", object@text)
})

