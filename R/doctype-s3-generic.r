
usage.s3generic <- usage.function

doctype.s3method <- function(obj) "s3"

default_export.s3generic <- function(obj, name) {
  all <- all_s3_methods(environment(obj))
  matching <- all[all[, 1] == name, ]
  
  list(S3method = matching, export = name)
}
