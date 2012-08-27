#' @export
#' @dev
rocout <- function(tag, name, subclass) {
  structure(list(tag = tag, name = name), class = c(subclass, "rocout"))
}


output_postproc <- function(output) {
  UseMethod("output_postproc")
}
output_write <- function(commands, path) {
  UseMethod("output_write")
}

#' @export
output_path <- function(writer, rocblock) {
  UseMethod("output_path")
}
