rocout <- function(tag, name, subclass) {
  structure(list(tag = tag, name = name), class = c(subclass, "rocout"))
}


write_output <- function(writer, rocblocks) {
  UseMethod("write_output")
}

output_path <- function(writer, rocblock) {
  UseMethod("output_path")
}

output_type <- function(writer, rocblock) {
  UseMethod("output_type")
}

# @return a list of the same length as \code{rocblocks}
process_output <- function(rocout, rocblocks) {
  # Name should have been set by roccer
  stopifnot(!is.null(rocout$name))
  
  lapply(rocblocks, function(rocblock) {
    tag <- rocblock$roc[[rocout$name]]
    if (is.null(tag)) return()
    rocout$tag(tag)
  })
}

