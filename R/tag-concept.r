setClass("TagConcept", contains = "Tag")
setMethod("procTag", "TagConcept", function(tag) {
  parse_words(tag)
})
