#' @@note: Add a note section.
#'
#' Use this for special notes that the user should be aware of.
#'
#' @tagUsage @@note
#'   This text will be added to a note section.
#'
#'   You can use multiple paragraphs.
setClass("NoteTag", contains = "Tag")
setMethod("writeRd", "NoteTag", function(object) {
  RdCommand("note", object@text)
})
