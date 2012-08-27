usage.roccer <- function(...) NULL

doctype.roccer <- function(x) "roccer"

object_from_call_roccer <- function(call, name, env) {
  name <- str_c("@", call$name)
  val <- get(name, env)  
  list(name = name, value = val)
}
object_from_call.add_roccer <- object_from_call_roccer
object_from_call.add_tag_roccer <- object_from_call_roccer
object_from_call.add_ns_roccer <- object_from_call_roccer


#' @auto_imports
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
