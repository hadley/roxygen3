doctype.s3method <- function(obj) "s3"

doctype_s3 <- function(roc, obj, ...) {
  method <- find_generic(obj$name, environment(obj$value))
  
  list(method = method)
}
