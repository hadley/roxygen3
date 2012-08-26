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
