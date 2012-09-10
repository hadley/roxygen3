#' Tag class
#' 
#' The tag class is the base class for all roxygen3 tags. 

setMethod("show", "Tag", function(object) {
  tag <- tag_name(object)
  
  out <- str_c("@", tag, " ", str_c(object@text, collapse = "\n"))
  cat(str_truncate(out), "\n", sep = "")
})

setMethod("procBlock", "Tag", function(tag, block) tag)
setMethod("procTag", "Tag", function(tag) tag)

setMethod("getPrereqs", "Tag", function(tag) {
  character()
})


tag_name <- function(x) {
  class <- getClass(class(x))@className
  first_lower(str_replace(class, "^Tag", ""))
}