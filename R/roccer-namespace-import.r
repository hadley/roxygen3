#' Namespace: tags for importing functions.
#' 
#' By and large, \code{@@auto_imports} should be the only imports 
#' tag that you need to use. It automatically generates the necessary
#' \code{importFrom} statements to import all external functions used by this
#' function.  See \code{\link{auto_imports}} for more implementation details.
#'
#' If there is a conflict, use \code{tag_importFrom} to resolve it. You can do
#' \code{@@importFrom base function} - this is not normally allowed in the
#' \file{NAMESPACE}, but roxygen3 will simply ignore it, but still use it when
#'  resolving conflicts.
#' 
#' You must have the packages declared in \code{DESCRIPTION} Imports.
#' 
#' 
#' @usage @@importFrom package function1 function2
#' @rdname tag-import
#' @auto_import
add_roccer("importFrom", 
  roc_parser(
    function(tag, name) {
      pieces <- str_split(tag, "[[:space:]]+")[[1]]
      if (length(pieces) < 2) {
        stop("@importFrom needs at least two components.", call. = FALSE)
      }
      setNames(rep(pieces[1], length(pieces[-1])), pieces[-1])
    }
  ),
  namespace_out(function(tag) {
    tag <- tag[tag != "base"]
    if (length(tag) == 0) return()
    
    str_c("importFrom(", tag, ",", quote_if_needed(names(tag)), ")", 
      collapse = "\n")
  })
)
base_prereqs[["auto_import"]] <- "importFrom"

#' @rdname tag-import
#' @usage @@auto_import
#' @auto_import
add_roccer("auto_import", 
  roc_parser(one = 
    function(roc, obj, ...) {
      if (is.null(roc$auto_import)) return()
      if (!is.function(obj$value)) return()
      
      auto <- auto_imports(obj$value, obj$name, roc$importFrom)
      list(importFrom = c(auto, roc$importFrom), auto_import = NULL)
    }
  )
)

#' @rdname tag-import
#' @usage @@import package1 package2 package3
add_ns_roccer("import", 
  words_tag(), 
  ns_each("import")
)
#' @rdname tag-import
#' @usage @@importClassesFrom package fun1 fun2
add_ns_roccer("importClassesFrom", 
  words_tag(), 
  ns_repeat1("importClassesFrom")
)
#' @rdname tag-import
#' @usage @@importMethodsFrom package fun1 fun2
add_ns_roccer("importMethodsFrom", 
  words_tag(), 
  ns_repeat1("importMethodsFrom")
)

