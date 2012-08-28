#' Generate usage statement for an R object.
#'
#' @param obj object to generate usage for
#' @param name name of object (if not computable from \code{obj})
#' @export
#' @dev
usage <- function(obj, name, ref) {
  UseMethod("usage")
}

new_usage <- function(..., subclass) {
  list(structure(list(...), class = c(subclass, "usage")))
}

#' @export
print.usage <- function(x, ...) {
  print(format(x))
}

usage.data.frame <- function(obj, name, ref) {
  name
}

usage.NULL <- function(obj, name, ref) NULL

usage.default <- function(obj, name, ref) {
  message("No usage method defined for object of class ", 
    str_c(class(obj), collapse = ", "), ref_location(ref))
  NULL
}

ref_location <- function(srcref) {
  if (is.null(srcref)) return("")
  
  file <- getSrcFilename(srcref)
  str_c(" @", file, ":", srcref[1], ":", srcref[5])
}