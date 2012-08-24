ns_roccer <- function(name, input, output) {
  roccer(name, 
    roc_parser(tag = input),
    namespace_out(output))
}

ns_roccer("import", words_tag(), ns_each("import"))
ns_roccer("importFrom", words_tag(), ns_repeat1("importFrom"))
ns_roccer("importClassesFrom", words_tag(), ns_repeat1("importClassesFrom")) 
ns_roccer("useDynLib", arguments_tag(), ns_directive("useDynLib"))

# But how would you document these? Generally, how to do you create and
# document bundles of related roccers?

ns_each <- function(directive) {
  function(values) {
    lines(directive, "(", values, ")")
  }
}
ns_call <- function(directive) {
  function(values) {
    args <- paste(names(values), " = ", values, collapse = ", ", sep = "")
    lines(directive, "(", args, ")")
  }
}
ns_repeat1 <- function(directive) {
  function(values) {
    lines(directive, "(", values[1], ",", values[-1], ")")
  }
}

lines <- function(...) paste(..., sep = "", collapse = "\n")

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