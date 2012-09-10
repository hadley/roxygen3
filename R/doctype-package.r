
#' @autoImports
doctype_package <- function(roc, ...) {
  name <- roc$name
  
  out <- list(docType = "package")
  if (!str_detect(name, "-package")) {
    out$aliases <- c(roc$aliases, str_c(name, "-package"))
  }
  out
}

