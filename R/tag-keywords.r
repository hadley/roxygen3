setClass("KeywordsTag", contains = "Tag")

setMethod("defaultTag", c("KeywordsTag", "DataObject"), function(tag, object) {
  new("KeywordsTag", text = "datasets")
})

setMethod("procTag", "KeywordsTag", function(tag) {
  parse_words(tag)
})

setMethod("writeRd", "KeywordsTag", function(object) {
  new_command("keyword", object@text)
})

