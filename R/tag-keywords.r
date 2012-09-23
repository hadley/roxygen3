#' @@keywords: provide high-level keywords.
#'
#' @details
#' Roxygen3 will automatically add data, classes, and methods keywords for data,
#' objects (S4 and R5), and methods (S3 and S4).
#'
#' Functions with the internal keyword will not be listed in the default
#' package index file.
#'
#' @tagUsage @@keywords keyword1 keyword2 keyword3
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

