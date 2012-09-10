#' Output to Rd files in the man directory.
#'
#' This uses the \code{@@rdname} tag to determine which file the output from
#' each rocblock is sent to.
#' 
#' Only one of \code{tag} and \code{out} can be supplied.
#'
#' @param tag a function that takes a single argument (tag) as input, and
#'   returns a list of \code{rd_commands} as output
#' @param name input tag name, usually set by \code{\link{roccer}}.
#' @dev
#' @export
setMethod("writeRd", "RoxyPackage", function(object) {
  in_dir(object@path, callNextMethod())
})
setMethod("writeRd", "RoxyBundle", function(object) {
  rd <- build_rd(object@blocks)
  Map(write_rd, rd, names(rd))
})
setMethod("writeRd", "RoxyBlock", function(object) {
  compact(lapply(object@tags, writeRd))
})
setMethod("writeRd", "Tag", function(object) NULL)

build_rd <- function(blocks) {
  commands <- lapply(blocks, writeRd)
  has_command <- vapply(commands, function(x) length(x) > 0, logical(1))

  paths <- lapply(blocks, output_path)
  has_path <- vapply(paths, Negate(is.null), logical(1))
  
  # Only write files with both path and contents
  complete <- has_command & has_path
  commands <- commands[complete]
  paths <- unlist(paths[complete])
  
  compact(tapply(commands, paths, collapse_rd))
}

output_path <- function(block) {
  tags <- names(block@tags)
  if ("noRd" %in% tags) return()
  
  rdname <- block@tags$rdname
  if (is.null(rdname)) {
    # message("rdname not specified, skipping.")
    return()
  }
  file.path("man", paste(rdname@text, ".Rd", sep = ""))
}

collapse_rd <- function(blocks) {
  commands <- unlist(blocks, recursive = FALSE)
  command_names <- vapply(commands, "[[", "command", FUN.VALUE = character(1))

  # Must have at least name and title to generate a file
  if (!all(c("title", "name") %in% command_names)) return()

  # Merge matching tags
  if (anyDuplicated(command_names)) {
    dedup <- list()
    for (i in seq_along(command_names)) {
      existing <- dedup[[command_names[i]]]
      if (is.null(existing)) {
        dedup[[command_names[i]]] <- commands[[i]]
      } else {
        dedup[[command_names[i]]] <- merge(existing, commands[[i]])
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
  
  commands
}

write_rd <- function(commands, path) {
  if (length(commands) == 0) return()
  
  formatted <- vapply(commands, "format", character(1))
  if (write_if_different(path, formatted)) {
    try(checkRd(path))
  }
}
