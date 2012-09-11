setClass("TagEncoding", contains = "Tag")
setMethod("procTag", "TagEncoding", function(tag) {
  parse_words(tag, 1, 1)
})
