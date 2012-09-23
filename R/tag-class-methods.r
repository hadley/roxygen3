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
  if (!is(block@object, "S4ClassObject")) {
    message("@classMethods only valid for documenting S4 classes ",
      location(block))
    return(block)
  }

  obj <- block@object@value
  methods <- class_methods(obj)
  if (nrow(methods) == 0) return(block)

  star <- ifelse(methods$self, "", "*")
  nmethods <- ifelse(methods$subclasses == 0, "",
    str_c(" (", methods$subclasses, " methods defined for subclasses)"))

  items <- str_c("  \\item \\code{\\link{", methods$generic, "}}",
    star, nmethods, collapse = "\n")
  title <- str_c("Generics with methods for ", obj@className)
  content <- str_c("\\itemize{\n", items, "\n}\n")
  if (any(!methods$self)) {
    content <- str_c(content, "\n* = methods only defined for subclasses")
  }

  section <- tag(block, "section")
  section@sections <- c(section@sections, setNames(content, title))
  tag(block, "section") <- section
  block
})

get_generics <- function() {
  generics <- getGenerics()@.Data
  is_generic <- vapply(generics, isGeneric, logical(1))
  generics[is_generic & generics != "coerce"]
}

class_methods <- function(class) {
  if (is.character(class)) class <- getClass(class)

  gen <- get_generics()
  sub <- names(sub_classes(class))

  sub <- vapply(gen, n_methods, sub = sub, integer(1))
  own <- vapply(gen, n_methods, sub = class@className, integer(1)) == 1

  df <- data.frame(generic = gen, self = own, subclasses = sub, row.names = NULL)
  df[df$self | df$subclasses > 0, ]
}
n_methods <- function(gen, sub) {
  sigs <- findMethods(gen)@signatures
  sum(vapply(sigs, function(x) any(sub %in% x), logical(1)))
}

