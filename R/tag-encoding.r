setClass("EncodingTag", contains = "Tag")
setMethod("procTag", "EncodingTag", function(tag) {
  tag@text <- words_tag(1, 1)(tag)
  tag
})
