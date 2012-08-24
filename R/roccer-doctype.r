roc_doctype <- roccer("docType",
  roc_parser(
    words_tag(1, 1),
    one = function(roc, obj, ...) {
      doctype <- roc$docType %||% doctype(obj$value)
      if (is.null(doctype)) return(list())
      
      out <- list()
      if (doctype == "package") {
        name <- roc$name
        if (!str_detect(name, "-package")) {
          out$aliases <- c(out$aliases, str_c(name, "-package"))
        }
      } else if (doctype == "data") {
        out$format <- roc$format %||% format_string(obj$value)
        out$tags <- c(out$keyword, "datasets")
      }

      out
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
