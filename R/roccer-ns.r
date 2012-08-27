
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