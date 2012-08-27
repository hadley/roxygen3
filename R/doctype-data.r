doctype.data.frame <- function(obj) "data"

doctype_data <- function(roc, obj, ...) {
  list(
    format = roc$format %||% format_string(obj$value),
    keywords = c(roc$keywords, "datasets"),
    docType = "data"
  )
}

format_string <- function(obj) {
  str_c(capture.output(str(obj, max.level = 1)), collapse = "\n")
}
