#' @rdname tag-import
#' @usageTag @@import package1 package2 package3
setClass("ImportTag", contains = "Tag")
setMethod("value<-", "ImportTag", function(tag, value) {
  tag@text <- str_split(value, " ")[[1]]
  tag
})
setMethod("writeNamespace", "ImportTag", function(object) {
  ns_each("import", object@text)
})
