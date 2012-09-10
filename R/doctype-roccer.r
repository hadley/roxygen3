usage.roccer <- function(...) NULL

doctype.roccer <- function(x) "roccer"

#' @autoImports
doctype_roccer <- function(roc, obj, ...) {
  if (!is.roccer(obj$value)) return()
  out <- list()
  
  bare_name <- str_replace(obj$name, "@", "")
  out$rdname <- roc$rdname %||% str_c("tag-", bare_name)
  out$name <- roc$name %||% str_c("tag_", bare_name)
  out$aliases <- str_c("tag_", bare_name)
  out$docType <- "roccer"
  
  if (!is.null(roc$usage)) {
    out$usage <- str_c("#' ", roc$usage)
  }
  out
}
