setClass("TagKeywords", contains = "Tag")
setMethod("procTag", "TagKeywords", function(tag) {
  parse_words(tag)
})
setMethod("writeRd", "TagKeywords", function(object) {
  new_command("keyword", object@text)
})
