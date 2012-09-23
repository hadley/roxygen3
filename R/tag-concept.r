#' Add a "concept" index entry.
#'
#' These can be used to add additional entries that will be searched for
#' in \code{\link{help.search}}.
#'
#' @tagUsage @@concept additional index entry
setClass("ConceptTag", contains = "Tag")
setMethod("value<-", "ConceptTag", function(tag, value) {
  tag@text <- unlist(str_split(value, "[[:space:]]+"))
  tag
})
