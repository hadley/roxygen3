
#' Given an object, return a string showing how that object should be
#' used.
#'
#'

setMethod("usage", "ANY", function(value, name, srcref) {
  message("No usage method defined for object of class ", 
    str_c(class(value), collapse = ", "), ref_location(srcref))
  NULL
})

setMethod("usage", "NULL", function(value, name, srcref) {
  NULL
})
