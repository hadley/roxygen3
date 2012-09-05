#' Tag class
#' 
#' The tag class is the base class for all roxygen3 tags. 
setClass("Tag",
  representation(text = "character", srcref = "srcref", "VIRTUAL"))

setMethod("print", "Tag", function(x, indent = 0, ...) {
  class <- x@className
  width <- getOptions(width) - nchar(class) - 2 - indent
  
  cat(str_dup(" ", indent), class, ": ", str_sub(x@text, 1, width))
})

setMethod("procBlock", "Tag", function(tag, block) tag)
setMethod("procTag", "Tag", function(tags) tag)

setMethod("getPrereqs", "Tag", function(tag) {
  character()
})