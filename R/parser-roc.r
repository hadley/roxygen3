#' A roc parser.
#'
#' This parser focus on access at the roc level, sacrificing generality for
#' simplicity. It is suitable for tags that need only local access, modifying
#' only roc component of the rocblock where the tag is located.
#'
#' @section Parsing order:
#' The functions \code{tag} and \code{one} are called in that
#' order, so if multiple are supplied, each can rely on having access to
#' the results of the previous.  See \code{\link{tag_name}} for an example
#' of this.
#'
#' @param tag a function that takes a single argument and returns an 
#'   object that will be used 
#' @param one a function with named arguments corresponding to any component
#'   of a rocblock (e.g. \code{roc}, \code{obj}, \code{path}) that returns
#'   a named list specifying the modifications that should be made to the roc.
#' @param name when \code{NULL} will automatically be giving the name of the
#'   roccer that uses it.
#' @dev
#' @export
roc_parser <- function(tag = NULL, one = NULL, all = NULL, name = NULL) {
  tag_m <- if (!is.null(tag)) memoise(tag) else tag
  one_m <- if (!is.null(one)) memoise(one) else one
  
  structure(list(tag = tag_m, one = one_m), class = "roc_parser")
}

parse_rocblocks.roc_parser <- function(parser, rocblocks) {
  # Name should have been set by roccer
  stopifnot(!is.null(parser$name))
  
  # Loop through all rocblocks, extracting tag. 
  if (!is.null(parser$tag)) {
    for(i in seq_along(rocblocks)) {
      roc <- rocblocks[[i]]$roc      
      if (is.null(roc)) next
      
      tag <- roc[[parser$name]]
      if (is.null(tag)) next
      
      rocblocks[[i]]$roc[[parser$name]] <- parser$tag(tag, parser$name)
    }
  }
  
  # Loop through all rocblocks, calling parser with do.call.
  if (!is.null(parser$one)) {
    for(i in seq_along(rocblocks)) {
      if (is.null(rocblocks[[i]]$roc)) next
      
      out <- do.call(parser$one, rocblocks[[i]])
      rocblocks[[i]]$roc <- modify_list(rocblocks[[i]]$roc, out)
    }
  }
  
  rocblocks
}

# Useful tag parsing functions -----------------------------------------------

split_pieces <- function(text, key, split_with, min, max) {
  pieces <- unlist(str_split(text, split_with))
  
  if (length(pieces) < min) {
    stop(key, " requires at least ", min, " values.")
  } 
  if (length(pieces) > max) {
    stop(key, " takes at most ", max, " values.")
  }

  pieces
}

words_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, key, "[[:space:]]+", min, max)
  }
}

arguments_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, key, ", ?", min, max)
  }
}

text_tag <- function() {
  function(text, key, ...) text
}

name_desc_tag <- function() {
  function(text, key, srcref) {
    pieces <- str_split_fixed(text, "[[:space:]]+", 2)
  
    name <- pieces[, 1]
    desc <- str_trim(pieces[, 2])

    if (any(name == "")) {
      stop(key, ' requires a name and description')
    }
    setNames(desc, name)
  }
}


