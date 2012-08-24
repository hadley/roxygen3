roc_usage <- roccer("usage",
  roc_parser(
    tag = text_tag(),
    one = function(roc, obj, ...) {
      if (!is.null(roc$usage)) return()
      
      if (!is.null(roc$method)) {
        # Manually munge name for s3 methods
        name <- str_c("\\method", str_c("{", roc$method, "}", collapse = ""))
      } else {
        name <- obj$name
      }
      list(usage = usage(obj$value, name))
    }
  ),
  rd_out(rd_command("usage"))
)

usage <- function(obj, name) {
  UseMethod("usage")
}
usage.function <- function(obj, name) {
  args <- usage_args(formals(obj))
  if (str_detect(name, fixed("<-"))) {
    usage <- str_c(name, "(", args, ") <- value")
  } else {
    usage <- str_c(name, "(", args, ")")
  }
}

usage.method <- function(obj, name) {
  signature <- str_c(as.character(obj@defined), collapse = ",")
  str_c("\\S4method{", obj@generic, "}{", signature, "}")
}

usage.data.frame <- function(obj, name) {
  name
}

usage.default <- function(obj, name) {
  message("No usage method defined for object of class ", 
    str_c(class(obj), collapse = ", "))
  NULL
}

# Given argument list, produce usage string for it.
# 
# Adapted from \code{\link{prompt}}.
#
# @param f function, or name of function, as string
# @return a string
usage_args <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    text <- str_replace_all(text, fixed("%"), "\\%")
    text <- str_replace_all(text, fixed(" "), "\u{A0}")
    Encoding(text) <- "UTF-8"    
    
    str_c("\u{A0}=\u{A0}", paste(text, collapse = "\n"))
  }
  
  arg_values <- vapply(args, arg_to_text, character(1))
  str_c(names(args), arg_values, collapse = ", ")
}
