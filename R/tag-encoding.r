setClass("TagEncoding", contains = "Tag")
setMethod("procTag", "TagEncoding", function(tag) {
  tag@text <- words_tag(1, 1)(tag)
  tag
})
