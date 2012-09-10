setClass("TagConcept", contains = "Tag")
setMethod("procTag", "TagConcept", function(tag) {
  tag@text <- words_tag()(tag)
  tag
})
