
# But how would you document these? Generally, how to do you create and
# document bundles of related roccers?

add_ns_roccer("import", 
  words_tag(), 
  ns_each("import")
)
add_ns_roccer("importClassesFrom", 
  words_tag(), 
  ns_repeat1("importClassesFrom")
)
add_ns_roccer("importMethodsFrom", 
  words_tag(), 
  ns_repeat1("importMethodsFrom")
)
add_ns_roccer("useDynLib", 
  arguments_tag(), 
  ns_each("useDynLib")
)
add_roccer("S3method",
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