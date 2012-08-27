#' Set object documentation type.
#' 
#' @details You can use any doctype, but it will only be included in
#'   the Rd file if it is one of the standard R doctypes: data, package,
#'   methods and class.
#'
#' @usage 
#'   @@docType date
#'   @@docType package
#'   @@docType custom doctype
#'
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

#' @S3method
format.docType_command <- function(x, ...) {
  vals <- unique(x$value)
  if (length(vals) != 1) stop("Documentation can only have single docType")

  ok <- c("data", "package", "methods", "class")
  vals <- intersect(vals, ok)
  if (length(vals) == 0) return("")
  
  str_c("\\docType{", x$value, "}")
}

