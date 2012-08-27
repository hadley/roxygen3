doctype.MethodDefinition <- function(obj) "method"

usage.MethodDefinition <- function(obj, name) {
  args <- usage_args(formals(obj))
  new_usage(
    args = usage_args(formals(obj@.Data)),
    generic = obj@generic,
    signature = obj@defined,
    subclass = "usage_s4method")
}

format.usage_s4method <- function(x, ...) {
  signature <- str_c(as.character(x$signature), collapse = ",")
  
  str_c("\\S4method{", x$generic, "}{", signature, "}(", 
    args_string(x$args), ")")
}

default_export.MethodDefinition <- function(obj, name) {
  list(exportMethods = as.vector(obj@generic), export = NULL)
}
