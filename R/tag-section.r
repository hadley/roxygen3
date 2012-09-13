#' @slot sections A named character vector.  The names represent the section
#'  titles and the values the contents.
#' @autoImports
setClass("TagSection", contains = "Tag",
  list(sections = "character")
)

setMethod("format", "TagSection", function(x, ...) x@sections %||% x@text)

setMethod("procTag", "TagSection", function(tag) {
  pieces <- str_split_fixed(tag@text, ":", n = 2)
  tag@sections <- setNames(pieces[, 2], pieces[, 1])
  tag
})

setMethod("writeRd", "TagSection", function(object) {
  new_command("section", object@sections)
})
