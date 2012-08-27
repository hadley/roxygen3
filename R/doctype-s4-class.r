#' @export
doctype.classRepresentation <- function(obj) "class"

default_export.classRepresentation <- function(obj, name) {
  list(exportClass = as.vector(obj@className), export = NULL)
}
