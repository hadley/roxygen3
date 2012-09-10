setClass("TagKeywords", contains = "Tag")
setMethod("procTag", "TagKeywords", function(tag) {
  tag@text <- words_tag()(tag@text)
  tag
})
setMethod("writeRd", "TagKeywords", function(object) {
  new_command("keyword", object@text)
})
