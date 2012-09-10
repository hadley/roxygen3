setClass("TagKeywords", contains = "Tag")
setMethod("procTag", "TagKeywords", function(tag) {
  tag@text <- words_tag()(tag)
  tag
})
setMethod("writeRd", "TagKeywords", function(tag) {
  new_command("keyword", tag@text)
})
