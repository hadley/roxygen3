#' @Section Extension
#' To automatically add new docTypes with their own defaults, implement
#' a method of \code{\link{doctype}} that returns a string, and function
#' \code{doctype_[doctype]} which works in the same way as a \code{one}
#' parser supplied to \code{\link{roc_parsers}}.
roc_doctype <- roccer("docType",
  roc_parser(
    words_tag(1, 1),
    one = function(roc, obj, ...) {
      doctype <- roc$docType %||% doctype(obj$value)
      if (is.null(doctype)) return()
      
      fname <- str_c("doctype_", doctype)
      default <- find_function(fname)
      if (is.null(default)) {
        message("No default doctype function ", str_c("doctype_", doctype), 
          " found.")
        return()
      }
      
      default(roc, obj, ...)
    }
  ),
  rd_out(rd_command("docType"))
)

doctype <- function(obj) UseMethod("doctype")
doctype.function <- function(obj) NULL
doctype.MethodDefinition <- function(obj) "method"
doctype.classRepresentation <- function(obj) "class"
doctype.default <- function(obj) "data"

format_string <- function(obj) {
  str_c(capture.output(str(obj, max.level = 1)), collapse = "\n")
}

doctype_package <- function(roc, ...) {
  name <- roc$name
  if (!str_detect(name, "-package")) {
    list(aliases = c(roc$aliases, str_c(name, "-package")))
  }
}

doctype_data <- function(roc, obj, ...) {
  list(
    format = roc$format %||% format_string(obj$value),
    tags = c(out$keyword, "datasets")
  )
}