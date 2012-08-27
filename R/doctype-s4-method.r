doctype.MethodDefinition <- function(obj) "method"

usage.MethodDefinition <- function(obj, name) {
  signature <- str_c(as.character(obj@defined), collapse = ",")
  
  str_c("\\S4method{", obj@generic, "}{", signature, "}")
}

default_export.MethodDefinition <- function(obj, name) {
  list(exportMethods = as.vector(obj@generic), export = NULL)
}
