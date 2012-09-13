#' @rdname tag-import
#' @usageTag @@importMethodsFrom package fun1 fun2
setClass("ImportMethodsFromTag", contains = "Tag")
setMethod("value<-", "ImportMethodsFromTag", function(tag, value) {
  tag@text <- str_split(value, " ")[[1]]
  tag
})
setMethod("writeNamespace", "ImportMethodsFromTag", function(object) {
  ns_repeat1("importMethodsFrom",object@text)
})
