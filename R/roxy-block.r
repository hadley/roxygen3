#' RoxyBlock class.
#'
#' The block class encapsulates the data about a single roxygen comment 
#' block, along with its location in the src code, and the object associated
#' with the block.
#'
#' @export
RoxyBlock <- function(tags, object, srcref) {
  if (is.null(tags$name) && !is.null(object@value)) {
    tags$name <- new("TagName", text = object@name, srcref = srcref)
  }
  if (is.null(tags$rdname) && !is.null(tags$name)) {
    tags$rdname <- new("TagRdname", text = nice_name(tags$name@text), 
      srcref = srcref)
  }
  
  new("RoxyBlock", tags = tags, object = object, srcref = srcref)
}

setMethod("show", "RoxyBlock", function(object) {
  cat("RoxyBlock: ", object@object@name, "@", 
    location(object@srcref), "\n", sep = "")
  lapply(object@tags, show)
})


# Convenience methods for modifying the tags in a block
modify_tags <- function(block, ...) {
  changes <- list(...)
  for(tag_name in names(changes)) {
    change <- changes[[tag_name]]
    if (is.null(change)) {
      block@tags[[tag_name]] <- NULL
    } else if (isS4(change)) {      
      block@tags[[tag_name]] <- change
    } else if (is.character(change)) {
      # stopifnot(length(change) == 1)
      block@tags[[tag_name]] <- modify_tag(block, tag_name, 
        list(text = change))
    } else if (is.list(change)) {
      block@tags[[tag_name]] <- modify_tag(block, tag_name, change)
    }
  }
  block
}
modify_tag <- function(block, tag_name, changes) {
  tag <- block@tags[[tag_name]]
  if (is.null(tag)) {
    tag <- find_tag(tag_name, "")
    if (is.null(tag)) stop("No ", tag_name)
  }
  
  for(slot_name in names(changes)) {
    old <- slot(tag, slot_name)
    slot(tag, slot_name) <- action(old, changes[[slot_name]])
  }
  tag
}
suffix <- function(x) structure(x, class = "suffix")
prefix <- function(x) structure(x, class = "prefix")
action <- function(old, new) {
  if (length(old) == 0) return(new)
  
  switch(class(new),
    prefix =    append(old, new, after = 0),
    suffix =    append(old, new),
    character = new
  )
}
