#' An output generator for the \file{NAMESPACE} file.
#'
#' @param tag function that processes a single tag. It should return a
#'   character vector of lines to be included in the \file{NAMESPACE}. 
#'   Duplicates will be automatically removed.
#' @param name input tag name, usually set by \code{\link{roccer}}.
#' @dev
#' @export
setMethod("writeNamespace", "RoxyPackage", function(object) {
  in_dir(object@path, callNextMethod())
})
setMethod("writeNamespace", "RoxyBundle", function(object) {
  ns <- build_namespace(object@blocks)
  write_namespace(ns)
})
setMethod("writeNamespace", "RoxyBlock", function(object) {
  compact(lapply(object@tags, writeNamespace))
})
setMethod("writeNamespace", "Tag", function(object) NULL)

build_namespace <- function(blocks) {
  lines <- unlist(lapply(blocks, writeNamespace), use.names = FALSE)
  with_collate("C", sort(unique(lines)))
}  

write_namespace <- function(ns) {
  write_if_different("NAMESPACE", ns)
}


# Useful output commands -----------------------------------------------------

ns_each <- function(directive) {
  function(values) {
    values <- values[values != ""]
    if (length(values) == 0) return()

    str_c(directive, "(", quote_if_needed(values), ")")
  }
}
ns_call <- function(directive) {
  function(values) {
    values <- values[values != ""]
    if (length(values) == 0) return()

    args <- paste(names(values), " = ", values, collapse = ", ", sep = "")
    str_c(directive, "(", args, ")")
  }
}
ns_repeat1 <- function(directive) {
  function(values) {
    values <- values[values != ""]
    if (length(values) == 0) return()

    str_c(directive, "(", quote_if_needed(values[1]), ",",
      quote_if_needed(values[-1]), ")")
  }
}
