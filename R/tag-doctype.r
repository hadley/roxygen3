#' Set object documentation type.
#' 
#' @details You can use any doctype, but it will only be included in
#'   the Rd file if it is one of the standard R doctypes: data, package,
#'   methods and class.
#'
#' @usage 
#'   @@docType data
#'   @@docType package
#'   @@docType custom doctype
#'
#' @Section Extension
#' To automatically add new docTypes with their own defaults, implement
#' a method of \code{\link{doctype}} that returns a string, and function
#' \code{doctype_[doctype]} which works in the same way as a \code{one}
#' parser supplied to \code{\link{roc_parsers}}.
setClass("TagDocType", contains = "Tag")

setMethod("defaultTag", c("TagDocType", "S4MethodObject"),
  function(tag, object) {
    new("TagDocType", text = "methods")
  }
)
setMethod("defaultTag", c("TagDocType", "S4ClassObject"),
  function(tag, object) {
    new("TagDocType", text = "class")
  }
)
setMethod("defaultTag", c("TagDocType", "DataObject"),
  function(tag, object) {
    new("TagDocType", text = "data")
  }
)
setMethod("defaultTag", c("TagDocType", "R5ClassObject"),
  function(tag, object) {
    new("TagDocType", text = "class")
  }
)
setMethod("defaultTag", c("TagDocType", "PackageObject"),
  function(tag, object) {
    new("TagDocType", text = "package")
  }
)

setMethod("writeRd", "TagDocType", function(object) {
  new_command("docType", object@text)
})

#' @export
format.docType_command <- function(x, ...) {
  vals <- unique(x$value)
  if (length(vals) != 1) stop("Documentation can only have single docType")

  ok <- c("data", "package", "methods", "class")
  vals <- intersect(vals, ok)
  if (length(vals) == 0) return("")
  
  str_c("\\docType{", vals, "}")
}

