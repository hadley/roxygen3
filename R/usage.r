#' Generate usage statement for an R object.
#'
#' @param obj object to generate usage for
#' @param name name of object (if not computable from \code{obj})
#' @export
usage <- function(obj, name) {
  UseMethod("usage")
}

new_usage <- function(..., subclass) {
  structure(list(...), class = c(subclass, "usage"))
}

#' @export
print.usage <- function(x, ...) {
  print(format(x))
}

usage.MethodDefinition <- function(obj, name) {
  signature <- str_c(as.character(obj@defined), collapse = ",")
  str_c("\\S4method{", obj@generic, "}{", signature, "}")
}

usage.data.frame <- function(obj, name) {
  name
}

usage.NULL <- function(obj, name) NULL

usage.default <- function(obj, name) {
  message("No usage method defined for object of class ", 
    str_c(class(obj), collapse = ", "))
  NULL
}


