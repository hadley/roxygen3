#' @@classMethods: automatically list all methods of an S4 class.
#'
#' This lists all methods connected to this class, directly, not through
#' any of its superclasses.
#'
#' In S4 methods are not associated with classes, but with generic functions.
#' This tag works by looking at all methods for generic function, and then
#' only showing those that use this class in at least one element of the
#' signature.
#'
#' @tagUsage
#'   @@classMethods
#'
setClass("ClassMethodsTag", contains = "Tag")

setMethod("process", "ClassMethodsTag", function(input, block) {
  if (!is(block@object@value, "classRepresentation")) {
    message("@classMethods only valid for documenting S4 classes ",
      location(block))
    return(block)
  }

  obj <- block@object@value
  methods <- class_methods(obj)

  if (length(methods) == 0) return(block)

  items <- str_c("  \\item ", describe(methods), collapse = "\n")
  title <- str_c("Generics with methods for ", obj@className)
  content <- str_c("\\itemize{\n", items, "\n}\n")

  section <- tag(block, "section")
  section@sections <- c(section@sections, setNames(content, title))
  tag(block, "section") <- section
  block
})

class_methods <- function(class) {
  if (is.character(class)) class <- getMethod(class)

  generics <- getGenerics()@.Data
  is_generic <- vapply(generics, isGeneric, logical(1))
  ok <- generics[is_generic & generics != "coerce"]

  methods_lists <- lapply(ok, findMethods, classes = class@className)
  unlist(lapply(methods_lists, "slot", ".Data"))
}
