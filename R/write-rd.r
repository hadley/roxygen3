#' Output to Rd files in the man directory.
#'
#' This uses the \code{@@rdname} tag to determine which file the output from
#' each rocblock is sent to.
#'
#' @dev
#' @export
#' @rdname writeRd
#' @param object Object to proccess, starting at a \linkS4class{Bundle},
#'   breaking down into \linkS4class{Block}s then individual
#'   \linkS4class{Tag}s
NULL

setMethod("writeRd", "PackageBundle", function(object) {
  in_dir(object@path, callNextMethod())
})
setMethod("writeRd", "Bundle", function(object) {
  rd <- build_rd(object@blocks)
  Map(write_rd, rd, names(rd))
})
setMethod("writeRd", "Block", function(object) {
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
  if (is.null(rdname)) return()

  file.path("man", paste(rdname@text, ".Rd", sep = ""))
}

collapse_rd <- function(blocks) {
  commands <- unlist(blocks, recursive = FALSE)
  command_names <- vapply(commands, "slot", "name", FUN.VALUE = character(1))

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
        dedup[[command_names[i]]] <- merge_rd(existing, commands[[i]])
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

#' @autoImports
write_rd <- function(commands, path) {
  if (length(commands) == 0) return()

  formatted <- unlist(lapply(commands, "format"))
  formatted <- c(built_by(), formatted)

  if (write_if_different(path, formatted)) {
    try(checkRd(path))
  }
}

built_by <- function() {
  pkg <- getPackageName()
  ver <- packageVersion(pkg)

  str_c("% Built by ", pkg, " ", format(ver))
}
