#' @@section: Add a new named section.
#'
#' This tag allows you to add arbitrarily named new sections. The section name
#' should not use any special formatting for latex compatibility.
#'
#' @tagUsage Section title:
#'   Section contents.  This can be multiple paragraphs.
#' @slot sections A named character vector. The names represent the section
#'  titles and the values the contents.
#' @autoImports
setClass("SectionTag", contains = "Tag",
  list(sections = "character")
)

setMethod("value", "SectionTag", function(tag) tag@sections)

setMethod("value<-", "SectionTag", function(tag, value) {
  pieces <- str_split_fixed(value, ":", n = 2)
  tag@sections <- setNames(pieces[, 2], pieces[, 1])
  tag
})

setMethod("writeRd", "SectionTag", function(object) {
  RdCommand("section", object@sections)
})
