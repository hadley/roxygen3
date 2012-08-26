#' @S3method
usage.roccer <- function(...) NULL

#' @S3method
doctype.roccer <- function(x) "roccer"
doctype_roccer <- function(roc, obj, ...) {
  if (!is.roccer(obj$value)) return()
  out <- list()
  
  out$rdname <- out$rdname %||% 
    str_c("tag-", str_replace(obj$name, "@", ""))
  out$docType <- "roccer"
  
  if (!is.null(roc$usage)) {
    out$usage <- str_c("#' ", roc$usage)
  }
  out
}
