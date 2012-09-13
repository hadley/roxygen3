setClass("ConceptTag", contains = "Tag")
setMethod("procTag", "ConceptTag", function(tag) {
  parse_words(tag)
})
