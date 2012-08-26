ns_roccer <- function(name, input, output) {
  roccer(name, 
    roc_parser(tag = input),
    namespace_out(output))
}

# But how would you document these? Generally, how to do you create and
# document bundles of related roccers?

ns_import <- ns_roccer(
  "import", 
  words_tag(), 
  ns_each("import")
)
ns_import_classes_from <- ns_roccer(
  "importClassesFrom", 
  words_tag(), 
  ns_repeat1("importClassesFrom")
)
ns_import_methods_from <- ns_roccer(
  "importMethodsFrom", 
  words_tag(), 
  ns_repeat1("importMethodsFrom")
)
ns_use_dyn_lib <- ns_roccer(
  "useDynLib", 
  arguments_tag(), 
  ns_each("useDynLib")
)
ns_s3_method <- roccer("S3method",
  roc_parser(
    words_tag(0, 2),
    one = function(roc, obj, ...) {
      n <- length(roc$S3method)
      if (n == 0) return()
      if (n == 2) return()
      
      if (roc$S3method == "") {
        # Empty, so guess from name
        pieces <- str_split_fixed(obj$name, fixed("."), n = 2)[1, ]
        generic <- pieces[1]
        class <- pieces[2]
      } else {
        generic <- roc$S3method
        class <- str_replace(obj$name, fixed(str_c(generic, ".")), "")
      }
      list(S3method = c(generic, class))
  }),
  namespace_out(function(methods) {
    if (is.vector(methods)) methods <- matrix(methods, ncol = 2)
    
    str_c("S3method(", quote_if_needed(methods[, 1]), ",",
      quote_if_needed(methods[, 2]), ")", collapse = "\n")
  })
)

ns_export_class <- ns_roccer(
  "exportClass", 
  words_tag(), 
  ns_each("exportClass")
)
ns_export_methods <- ns_roccer(
  "exportMethods", 
  words_tag(), 
  ns_each("exportMethods")
)
ns_export_pattern <- ns_roccer(
  "exportPattern", 
  words_tag(), 
  ns_each("exportPattern")
)

ns_export <- roccer("export",
  roc_parser(
    words_tag(),
    function(roc, obj, ...) {
      # Not specified, or ot empty, so just return
      if (is.null(roc$export) || roc$export != "") {
        return()
      }
      
      default_export(obj$value, obj$name)
    }
  ),
  namespace_out(ns_each("export"))
)
base_prereqs[["export"]] <- c("S3method", "docType")


# Also need to think about more consistent naming scheme:
# 
# @ns_dynlib
# @ns_method_s3
# @ns_import_from
# @ns_import_classes_from 
# 
# Would be fairly easy to write wrapper function so that 
# old names continue to work, but give deprecation message:
# 
# new_name("ns_dyn_lib", dynlib_tag())