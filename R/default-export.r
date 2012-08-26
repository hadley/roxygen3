#' @export
default_export <- function(obj, name) {
  UseMethod("default_export")
}

default_export.classRepresentation <- function(obj, name) {
  list(exportClass = as.vector(obj@className), export = NULL)
}

default_export.MethodDefinition <- function(obj, name) {
  list(exportMethods = as.vector(obj@generic), export = NULL)
}

default_export.function <- function(obj, name) {
  list(export = name)
}

default_export.s3method <- function(obj, name) {
  list(S3method = s3_method_info(obj), export = NULL)
}

default_export.s3generic <- function(obj, name) {
  all <- all_s3_methods(environment(obj))
  matching <- all[all[, 1] == name, ]
  
  list(S3method = matching, export = name)
}
