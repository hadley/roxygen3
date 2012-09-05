setClass("ConceptTag", contains = "Tag")
setMethod("procTag", "ConceptTag", function(tag) {
  tag@text <- words_tag()(tag)
  tag
})
