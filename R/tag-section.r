#' @slot sections A named character vector.  The names represent the section
#'  titles and the values the contents.
#' @autoImports
setClass("SectionTag", contains = "Tag",
  list(sections = "character")
)

setMethod("format", "SectionTag", function(x, ...) x@sections %||% x@text)

setMethod("procTag", "SectionTag", function(tag) {
  pieces <- str_split_fixed(tag@text, ":", n = 2)
  tag@sections <- setNames(pieces[, 2], pieces[, 1])
  tag
})

setMethod("writeRd", "SectionTag", function(object) {
  new_command("section", object@sections)
})
