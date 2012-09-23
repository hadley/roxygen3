#' @@genericMethods: automatically list all methods of a generic
#'
#' @tagUsage
#'   @@genericMethods
#'
setClass("GenericMethodsTag", contains = "Tag")

setMethod("process", "GenericMethodsTag", function(input, block) {
  if (!is(block@object, "S4GenericObject")) {
    message("@genericMethods only valid for documenting S4 generics ",
      location(block))
    return(block)
  }

  obj <- block@object@value
  methods <- findMethods(obj)

  if (length(methods) == 0) return(block)

  desc <- function(x) str_c(describe(x@defined), collapse = ", ")

  items <- str_c("  \\item ", vapply(methods, desc, character(1)),
      collapse = "\n")
  title <- str_c("Method signatures for generic ", obj@generic)
  content <- str_c("\\itemize{\n", items, "\n}\n")

  section <- tag(block, "section")
  section@sections <- c(section@sections, setNames(content, title))
  tag(block, "section") <- section
  block
})
