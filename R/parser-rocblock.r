#' A rocblock parser.
#' 
#' The rocblock parser is extremely general: the \code{process} function
#' gets a list of all rocblocks and it returns the transformed output. This
#' means it can perform actions that need a global perspective, as well as 
#' providing the ability to add and delete rocblocks.
#'
#' @dev
#' @export
#' @param process A function that's given a list of all rocblocks and should
#'   return the data that the 
rocblock_parser <- function(process) {
  structure(list(process = process), class = "rocblock_parser")
}

parse_rocblocks.rocblock_parser <- function(parser, rocblocks) {
  parser$process(rocblocks)
}