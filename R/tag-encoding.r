setClass("EncodingTag", contains = "Tag")
setMethod("procTag", "EncodingTag", function(tag) {
  parse_words(tag, 1, 1)
})
