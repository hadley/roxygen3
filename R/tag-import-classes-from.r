#' @rdname tag-import
#' @tagUsage @@importClassesFrom package fun1 fun2
setClass("ImportClassesFromTag", contains = "Tag")
setMethod("value<-", "ImportClassesFromTag", function(tag, value) {
  tag@text <- str_split(value, " ")[[1]]
  tag
})
setMethod("writeNamespace", "ImportClassesFromTag", function(object) {
  ns_repeat1("importClassesFrom",object@text)
})
