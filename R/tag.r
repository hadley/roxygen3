#' Tag class
#' 
#' The tag class is the base class for all roxygen3 tags. 
#'
#' @section Parsing order:
#' The functions \code{procTag} and \code{procBlock} are called in that
#' order, so if both are supplied, \code{procBlock} can rely on the 
#' tag already being parsed.
setMethod("show", "Tag", function(object) {
  tag <- tag_name(object)
  
  out <- str_c("@", tag, " ", str_c(object@text, collapse = "\n"))
  cat(str_truncate(out), "\n", sep = "")
})

# Default behaviour for all tags: don't change and no prereqs.
setMethod("procBlock", "Tag", function(tag, block) block)
setMethod("procTag", "Tag", function(tag) tag)
setMethod("getPrereqs", "Tag", function(tag) {
  character()
})

tag_name <- function(x) {
  class <- getClass(class(x))@className
  first_lower(str_replace(class, "^Tag", ""))
}