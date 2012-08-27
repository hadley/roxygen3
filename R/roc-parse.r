#' Create a rocblock parser.
#'
#' @section Parsing order:
#' The functions \code{tag}, \code{one} and \code{all} are called in that
#' order, so if multiple are supplied, each can rely on having access to
#' the results of the previous.  See \code{\link{tag_name}} for an example
#' of this.
#'
#' @param tag a function that takes a single argument and returns an 
#'   object that will be used 
#' @param one a function with named arguments corresponding to any component
#'   of a rocblock (e.g. \code{roc}, \code{obj}, \code{path}) that returns
#'   a named list specifying the modifications that should be made to the roc.
#' @param all a function with one parameter that is giving a list of all
#'   rocblocks and should return a named list containing named list specifying
#'   changes to the rocs.
#' @param name when \code{NULL} will automatically be giving the name of the
#'   roccer that uses it.
#' @dev
roc_parser <- function(tag = NULL, one = NULL, all = NULL, name = NULL) {
  # tag <- memoise(tag)
  # one <- memoise(one)
  # all <- memoise(all)
  
  structure(list(tag = tag, one = one, all = all), 
    class = "roc_parser")
}

parse_rocblocks.roc_parser <- function(parser, rocblocks) {
  # Name should have been set by roccer
  stopifnot(!is.null(parser$name))
  
  # Loop through all rocblocks, extracting tag. 
  if (!is.null(parser$tag)) {
    for(i in seq_along(rocblocks)) {
      tag <- rocblocks[[i]]$roc[[parser$name]]
      if (is.null(tag)) next
      
      rocblocks[[i]]$roc[[parser$name]] <- parser$tag(tag, parser$name)
    }
  }
  
  # Loop through all rocblocks, calling parser with do.call.
  if (!is.null(parser$one)) {
    for(i in seq_along(rocblocks)) {
      out <- do.call(parser$one, rocblocks[[i]])
      rocblocks[[i]]$roc <- modify_list(rocblocks[[i]]$roc, out)
    }
  }
  
  # Parsing function should return named list specifying changes.
  if (!is.null(parser$all)) {
    out <- parser$all(rocblocks)
    rocblocks <- Map(modify_list, rocblocks, out)
  }
  
  rocblocks
}

