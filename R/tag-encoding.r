setClass("EncodingTag", contains = "Tag")
setMethod("value<-", "EncodingTag", function(tag, value) {
  tag@text <- parse_words(tag, value, 1, 1)
  tag
})
