setClass("KeywordsTag", contains = "Tag")

setMethod("defaultTag", c("KeywordsTag", "DataObject"), function(tag, object) {
  new("KeywordsTag", text = "datasets")
})

setMethod("value<-", "KeywordsTag", function(tag, value) {
  tag@text <- unlist(str_split(value, "[[:space:]]+"))
  tag
})

setMethod("writeRd", "KeywordsTag", function(object) {
  RdCommand("keyword", object@text)
})

