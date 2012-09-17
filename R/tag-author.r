#' Provide the object's author.
#'
#' A free text string describing the authors of the function.  This is
#' typically only necessary if the author is not the same as the package author.
#'
#' @usageTag @@author authors...
setClass("AuthorTag", contains = "Tag")
setMethod("writeRd", "AuthorTag", function(object) {
  RdCommand("author", object@text)
})
