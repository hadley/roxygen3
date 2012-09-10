#' Tag class
#' 
#' The tag class is the base class for all roxygen3 tags. 

setMethod("show", "Tag", function(object) {
  class <- getClass(class(object))@className
  width <- getOption("width") - nchar(class) - 2
  
  cat(class, ": ", str_sub(object@text, 1, width), sep = "")
})

setMethod("procBlock", "Tag", function(tag, block) tag)
setMethod("procTag", "Tag", function(tag) tag)

setMethod("getPrereqs", "Tag", function(tag) {
  character()
})