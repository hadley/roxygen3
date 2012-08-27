#' @S3method
usage.roccer <- function(...) NULL

#' @S3method
doctype.roccer <- function(x) "roccer"

#' @auto_imports
doctype_roccer <- function(roc, obj, ...) {
  if (!is.roccer(obj$value)) return()
  out <- list()
  
  bare_name <- str_replace(obj$name, "@", "")
  out$rdname <- out$rdname %||% str_c("tag-", bare_name)
  out$name <- out$name %||% str_c("tag_", bare_name)
  out$docType <- "roccer"
  
  if (!is.null(roc$usage)) {
    out$usage <- str_c("#' ", roc$usage)
  }
  out
}
