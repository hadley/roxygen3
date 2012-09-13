#' Convenience methods for manipulating tags in a block.
#'
#' @dev
#' @rdname tag-modify
#' @param block \code{\link{Block}} object to modify
#' @param tag_name name of the tag (as a length 1 character vector)
#' @param create if \code{TRUE} will create a new \code{Tag} of the appropriate
#'   type if one is not present.  If \code{FALSE}, it will return \code{NULL}.
#' @examples
#' block <- Block()
#' tag(block, "name")
#' tag(block, "name", create = FALSE)
#'
#' tag(block, "name") <- "myname"
#' block
#' tag(block, "aliases") <- tag(block, "name")@text
#' block
#' tag(block, "aliases") <- suffix("end")
#' block
#' tag(block, "aliases") <- prefix("start")
#' block
#' @export
tag <- function(block, tag_name, create = TRUE) {
  tag <- block@tags[[tag_name]]

  if (!is.null(tag) || !create) return(tag)

  tag <- build_tag(tag_name, character())
  if (is.null(tag)) stop("Can't find tag called ", tag_name)
  tag
}

tag_value <- function(block, tag_name) {
  tag <- tag(block, tag_name, create = FALSE)
  if (is.null(tag)) return(NULL)
  value(tag)
}
"tag_value<-" <- function(block, tag_name, value) {
  new_tag <- tag(block, tag_name)
  value(new_tag) <- value
  tag(block, tag_name) <- new_tag
}

#' @param value the replacement value. If it is a character string, it
#'   will replace the \code{text} slot. If it's a \code{suffix} object, it
#'   will be added to the end of the tag's text. If it's a \code{prefix}
#'   object it will be added to the beginning of the tag's text. Otherwise, the
#'   existing tag will be replaced with the new value.
#' @rdname tag-modify
#' @export
setGeneric("tag<-",
  function(block, tag_name, value) {
    standardGeneric("tag<-")
  },
  signature = "value"
)

setMethod("tag<-", "ANY", function(block, tag_name, value) {
  block@tags[[tag_name]] <- value
  block
})
setMethod("tag<-", "character", function(block, tag_name, value) {
  new_tag <- tag(block, tag_name, create = TRUE)
  value(new_tag) <- value
  tag(block, tag_name) <- new_tag
  block
})

setClass("Suffix", contains = "character")
setClass("Prefix", contains = "character")
#' @rdname tag-modify
#' @export
suffix <- function(x) new("Suffix", x)
#' @rdname tag-modify
#' @export
prefix <- function(x) new("Prefix", x)

setMethod("tag<-", "Suffix", function(block, tag_name, value) {
  if (length(value) == 0) return(block)

  old_text <- tag_value(block, tag_name)
  tag(block, tag_name) <- c(old_text, value@.Data)
  block
})
setMethod("tag<-", "Prefix", function(block, tag_name, value) {
  if (length(value) == 0) return(block)

  old_text <- tag_value(block, tag_name)
  tag(block, tag_name) <- c(value@.Data, old_text)
  block
})
