#' RoxyBlock class.
#'
#' The block class encapsulates the data about a single roxygen comment 
#' block, along with its location in the src code, and the object associated
#' with the block.
#'
#' @export

setMethod("show", "RoxyBlock", function(object) {
  path <- getSrcFilename(object@srcref)
  cat("rocblock: ", object@obj@name, "@",
    path, ":", object@srcref[1], ":", object@srcref[3], "\n", sep = "")
  lapply(object@tags, print, indent = 2)
})

# Convenience methods for modifying the tags in a block
modify_tags <- function(block, ...) {
  changes <- list(...)
  for(tag_name in names(changes)) {
    change <- changes[[tag_names]]
    if (is.null(changes)) {
      blocks@tags[[tag_name]] <- NULL
    } else if (isS4(change)) {      
      blocks@tags[[tag_name]] <- change
    } else if (is.character(change)) {
      stopifnot(length(change = 1))
      blocks@tags[[tag_name]] <- modify_tag(block, tag_name, 
        list(text = change))
    } else if (is.list(change)) {
      blocks@tags[[tag_name]] <- modify_tag(block, tag_name, change)
    }
  }
  blocks
}
modify_tag <- function(block, tag_name, changes) {
  tag <- block@tags[[tag_name]] %||% find_tag(tag_name) %||% return(NULL)
  
  for(slot in name(changes)) {
    slot(block, name) <- action(values[slot])(slot(block, name))
  }
  tag
}
suffix <- function(x) structure(x, class = "suffix")
prefix <- function(x) structure(x, class = "prefix")
action <- function(old) {
  switch(class(old) %||% "replace",
    prefix =  function(new) append(old, new, after = 0),
    suffix =  function(new) append(old, new),
    replace = function(new) new
  )
}
