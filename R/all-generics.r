setGeneric("usage", function(value, name, srcref) {
  standardGeneric("usage")
})

setGeneric("writeRd", function(tag) {
  standardGeneric("writeRd")
})
setGeneric("writeNamespace", function(tag) {
  standardGeneric("writeNamespace")
})
setGeneric("writeDescription", function(tag) {
  standardGeneric("writeDescription")
})
setGeneric("getPrereqs", function(tag) {
  standardGeneric("getPrereqs")
}, valueClass = "character")

setGeneric("isNull", function(x) standardGeneric("isNull"))
setMethod("isNull", "NULL", function(x) TRUE)
setMethod("isNull", "ANY", function(x) FALSE)
