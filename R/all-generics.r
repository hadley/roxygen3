setGeneric("usage", function(value, name, srcref) {
  standardGeneric("usage")
})

#' @rdname writeRd
#' @export
#' @genericMethods
setGeneric("writeRd", function(object) {
  standardGeneric("writeRd")
})

#' @rdname writeNamespace
#' @export
#' @genericMethods
setGeneric("writeNamespace", function(object) {
  standardGeneric("writeNamespace")
})

#' @rdname writeDescription
#' @genericMethods
setGeneric("writeDescription", function(object) {
  standardGeneric("writeDescription")
})

setGeneric("getPrereqs", function(tag) {
  standardGeneric("getPrereqs")
}, valueClass = "character")

#' Process bundles and blocks.
#'
#' @dev
#' @param input object to process
#' @param ... other arguments used by methods.
#' @genericMethods
setGeneric("process", function(input, ...) {
  standardGeneric("process")
})
cached_process <- memoise(function(input, ...) {
  process(input, ...)
})

#' Compute path to a bundle.
#'
#' @param bundle input bundle
#' @keywords internal
#' @genericMethods
setGeneric("rPath", function(bundle) {
  standardGeneric("rPath")
})

setGeneric("value<-", function(tag, value) {
  standardGeneric("value<-")
})
setGeneric("value", function(tag) {
  standardGeneric("value")
})
setMethod("value", "NULL", function(tag) NULL)

setGeneric("isEmpty", function(tag) {
  standardGeneric("isEmpty")
})



setGeneric("isNull", function(x) standardGeneric("isNull"))
setMethod("isNull", "NULL", function(x) TRUE)
setMethod("isNull", "ANY", function(x) FALSE)
setMethod("isNull", "NullUsage", function(x) TRUE)
setMethod("isNull", "NullSrcref", function(x) TRUE)

#' Generate default tag.
#'
#' @param tag class of tag
#' @param object \linkS4class{Object} to generate defaults for.
#' @dev
#' @genericMethods
setGeneric("defaultTag", function(tag, object) standardGeneric("defaultTag"))
