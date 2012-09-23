#' @@classMethods: automatically list all methods of an object
#'
#' @tagUsage
#'   @@classMethods # exclude inherited methods
#'   @@classMethods inherited # include inherited methods
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
