#' Convenience methods for manipulating tags in a block.
#'
#' @dev
#' @rdname tag-modify
#' @param block \code{\link{Block}} object to modify
#' @param tagname name of the tag (as a length 1 character vector)
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
tag <- function(block, tagname, create = TRUE) {
  tag <- block@tags[[tagname]]

  if (!is.null(tag) || !create) return(tag)

  tag <- build_tag(tagname, character())
  if (is.null(tag)) stop("Can't find tag called ", tagname)
  tag
}

#' @param value the replacement value. If it is a character string, it
#'   will replace the \code{text} slot. If it's a \code{suffix} object, it
#'   will be added to the end of the tag's text. If it's a \code{prefix}
#'   object it will be added to the beginning of the tag's text. Otherwise, the
#'   existing tag will be replaced with the new value.
#' @rdname tag-modify
#' @export
setGeneric("tag<-",
  function(block, tagname, value) {
    standardGeneric("tag<-")
  },
  signature = "value"
)

setMethod("tag<-", "ANY", function(block, tagname, value) {
  block@tags[[tagname]] <- value
  block
})
setMethod("tag<-", "character", function(block, tagname, value) {
  new_tag <- tag(block, tagname, create = TRUE)
  new_tag@text <- value
  tag(block, tagname) <- new_tag
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

setMethod("tag<-", "Suffix", function(block, tagname, value) {
  if (length(value) == 0) return(block)

  old_text <- tag(block, tagname)@text
  tag(block, tagname) <- c(old_text, value@.Data)
  block
})
setMethod("tag<-", "Prefix", function(block, tagname, value) {
  if (length(value) == 0) return(block)

  old_text <- tag(block, tagname)@text
  tag(block, tagname) <- c(value@.Data, old_text)
  block
})
