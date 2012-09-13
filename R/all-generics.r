setGeneric("usage", function(value, name, srcref) {
  standardGeneric("usage")
})

setGeneric("writeRd", function(object) {
  standardGeneric("writeRd")
})
setGeneric("writeNamespace", function(object) {
  standardGeneric("writeNamespace")
})
setGeneric("writeDescription", function(object) {
  standardGeneric("writeDescription")
})
setGeneric("getPrereqs", function(tag) {
  standardGeneric("getPrereqs")
}, valueClass = "character")

setGeneric("process", function(input) {
  standardGeneric("process")
})

setGeneric("value<-", function(tag, value) {
  standardGeneric("value<-")
})
setGeneric("value", function(tag) {
  standardGeneric("value")
})
setMethod("value", "NULL", function(tag) NULL)

setGeneric("isNull", function(x) standardGeneric("isNull"))
setMethod("isNull", "NULL", function(x) TRUE)
setMethod("isNull", "ANY", function(x) FALSE)
setMethod("isNull", "NullUsage", function(x) TRUE)
setMethod("isNull", "NullSrcref", function(x) TRUE)

setGeneric("defaultTag", function(tag, object) standardGeneric("defaultTag"))
