#'' @@references: add references to the literature.
#'
#' @tagUsage @@references Text citation for an important paper.
setClass("ReferencesTag", contains = "Tag")
setMethod("writeRd", "ReferencesTag", function(object) {
  RdCommand("references", object@text)
})
