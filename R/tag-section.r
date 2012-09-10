#' @slot sections A named character vector.  The names represent the section
#'  titles and the values the contents.
#' @autoImports
setClass("SectionTag", contains = "Tag", 
  list(sections = "character")
)

setMethod("procTag", "SectionTag", function(tag) {
  pieces <- str_split_fixed(tag@text, ":", n = 2)
  tag@sections <- setNames(pieces[, 2], pieces[, 1])
  tag
})

setMethod("writeRd", "SectionTag", function(tag) {
  new_command("section", tag@sections)
})
