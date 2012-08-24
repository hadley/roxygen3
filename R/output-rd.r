#' Output to Rd files in the man directory.
#'
#' This uses the \code{@rdname} tag to determine which file the output from
#' each rocblock is sent to.
#' 
#' Only one of \code{tag} and \code{out} can be supplied.
#'
#' @param tag a function that takes a single argument (tag) as input, and
#'   returns a list of \code{rd_commands} as output
#' @param name name of the input tag - this is usually not specified as it
#'   is filled in by \code{\link{roccer}} but is useful for testing.
#' @dev
rd_out <- function(tag, name = NULL) {
  rocout(tag, name, subclass = "rd_out")
}

output_path.rd_out <- function(writer, rocblock) {
  if (is.null(rocblock$roc$rdname)) {
    stop("rdname not specified", call. = FALSE)
  }
  file.path("man", paste(rocblock$roc$rdname, ".Rd", sep = ""))
}

output_type.rd_out <- function(writer) {
  "rd_write"
}

rd_command <- function(command) {
  function(tag) {
    new_command(command, tag)
  }
}
