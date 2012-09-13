setClass("ConceptTag", contains = "Tag")
setMethod("value<-", "ConceptTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})
