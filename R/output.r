#' @export
#' @dev
rocout <- function(tag, name, subclass) {
  structure(list(tag = tag, name = name), class = c(subclass, "rocout"))
}


#' @export
write_output <- function(writer, rocblocks) {
  UseMethod("write_output")
}

#' @export
output_path <- function(writer, rocblock) {
  UseMethod("output_path")
}

#' @export
output_type <- function(writer, rocblock) {
  UseMethod("output_type")
}
