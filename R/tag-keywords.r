setClass("KeywordsTag", contains = "Tag")

setMethod("defaultTag", c("KeywordsTag", "DataObject"), function(tag, object) {
  new("KeywordsTag", text = "datasets")
})

setMethod("defaultTag", c("KeywordsTag", "S4ClassObject"), function(tag, object) {
  new("KeywordsTag", text = "classes")
})

setMethod("defaultTag", c("KeywordsTag", "S4MethodObject"), function(tag, object) {
  new("KeywordsTag", text = "methods")
})

setMethod("defaultTag", c("KeywordsTag", "S4GenericObject"), function(tag, object) {
  new("KeywordsTag", text = "methods")
})

setMethod("defaultTag", c("KeywordsTag", "S3MethodObject"), function(tag, object) {
  new("KeywordsTag", text = "methods")
})

setMethod("defaultTag", c("KeywordsTag", "S3GenericObject"), function(tag, object) {
  new("KeywordsTag", text = "methods")
})

setMethod("value<-", "KeywordsTag", function(tag, value) {
  tag@text <- unlist(str_split(value, "[[:space:]]+"))
  tag
})

setMethod("writeRd", "KeywordsTag", function(object) {
  RdCommand("keyword", object@text)
})

