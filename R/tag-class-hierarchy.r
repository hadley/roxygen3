#' @@classHierarchy: describe class hierarchy for an S4 object.
#'
#' This tag will add a new section listing all the super classes and all the
#' subclasses of the object.  It will be compiled at the time that roxygen3
#' is run, and does not (yet) update dynamically as you load other packages and
#' define new classes
#'
#' @tagUsage @@classHierarchy
setClass("ClassHierarchyTag", contains = "Tag")

setMethod("process", "ClassHierarchyTag", function(input, block) {
  if (!is(block@object@value, "classRepresentation")) {
    message("@slot only valid for documenting S4 classes ", location(block))
    return(block)
  }

  obj <- block@object@value
  sub <- sub_classes(obj)
  sup <- sup_classes(obj)

  if (length(sub) == 0 && length(sup) == 0) return(block)

  sub_items <- str_c("\\item ", describe(sub), collapse = "\n")
  sup_items <- str_c("\\item ", describe(sup), collapse = "\n")

  title <- str_c("Class hierarchy for ", obj@className)
  content <- str_c(
    if (length(sub) > 0) str_c("Sub classes:\n\\itemize{\n", sub_items, "\n}\n"),
    if (length(sup) > 0) str_c("Super classes:\n\\itemize{\n", sup_items, "\n}\n")
  )

  section <- tag(block, "section")
  section@sections <- c(section@sections, setNames(content, title))
  tag(block, "section") <- section
  block
})

