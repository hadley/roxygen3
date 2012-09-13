setClass("KeywordsTag", contains = "Tag")

setMethod("defaultTag", c("KeywordsTag", "DataObject"), function(tag, object) {
  new("KeywordsTag", text = "datasets")
})

setMethod("value<-", "KeywordsTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})

setMethod("writeRd", "KeywordsTag", function(object) {
  new_command("keyword", object@text)
})

