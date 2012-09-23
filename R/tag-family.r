#' @@family: associate the function with a family to automatically add
#'  cross-references.
#'
#' All functions with the same family will have cross-references automatically
#' added to all other functions in the family.
#'
#' @tagUsage @@family the name of the family
setClass("FamilyTag", contains = "Tag")

setMethod("value<-", "FamilyTag", function(tag, value) {
  tag@text <- unique(str_trim(value))
  tag
})
