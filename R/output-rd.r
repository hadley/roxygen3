#' Output to Rd files in the man directory.
#'
#' This uses the \code{@@rdname} tag to determine which file the output from
#' each rocblock is sent to.
#' 
#' Only one of \code{tag} and \code{out} can be supplied.
#'
#' @param tag a function that takes a single argument (tag) as input, and
#'   returns a list of \code{rd_commands} as output
#' @param name name of the input tag - this is usually not specified as it
#'   is filled in by \code{\link{roccer}} but is useful for testing.
#' @dev
#' @export
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

rd_write <- function(output, out_path) {
  paths <- file.path(out_path, names(output))
  mapply(write_rdlist, paths, output)
}

#' @auto_imports
write_rdlist <- function(path, commands) {
  # Merge matching tags
  command_names <- vapply(commands, "[[", "command", FUN.VALUE = character(1))
  
  if (!any(command_names %in% "title")) return()
  
  if (anyDuplicated(command_names)) {
    dedup <- list()
    browser()
    for (i in seq_along(command_names)) {
      existing <- dedup[command_names[i]]
      if (is.null(existing)) {
        dedup[command_names[i]] <- commands[i]
      } else {
        dedup[command_names[i]] <- merge(existing, commands[i])
      }
    }
    commands <- dedup
  } else {
    names(commands) <- command_names
  }
  
  order <- c("docType", "encoding", "name", "alias", "title", "format",
    "source", "usage", "arguments", "value", "description", "details", "slot",
    "note", "section", "examples", "author", "references", "seealso",
    "concept", "keyword")
  commands <- commands[c(intersect(order, names(commands)),
    setdiff(names(commands), order))]
  
  formatted <- vapply(commands, "format", character(1))
  if (write_if_different(path, formatted)) {
    try(checkRd(path))
  }
}

# Useful output commands -----------------------------------------------------

rd_command <- function(command) {
  function(tag) {
    new_command(command, tag)
  }
}
