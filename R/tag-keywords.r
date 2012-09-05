setClass("KeywordsTag", contains = "Tag")
setMethod("procTag", "KeywordsTag", function(tag) {
  tag@text <- words_tag()(tag)
  tag
})
setMethod("writeRd", "KeywordsTag", function(tag) {
  new_command("keyword", tag@text)
})
