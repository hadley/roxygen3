#' @export
namespace_out <- function(tag, name = NULL)  {
  rocout(tag, name, subclass = "namespace_out")
}

output_path.namespace_out <- function(writer, rocblock) {
  "NAMESPACE" 
}
output_type.namespace_out <- function(writer) {
  "ns_write"
}

ns_write <- function(output, out_path) {
  out <- file.path(out_path, "NAMESPACE")  
  formatted <- str_c(unlist(output), collapse = "\n")
  write_if_different(out, formatted)
}


# Useful output commands -----------------------------------------------------

ns_each <- function(directive) {
  function(values) {
    lines(directive, "(", values, ")")
  }
}
ns_call <- function(directive) {
  function(values) {
    args <- paste(names(values), " = ", values, collapse = ", ", sep = "")
    lines(directive, "(", args, ")")
  }
}
ns_repeat1 <- function(directive) {
  function(values) {
    lines(directive, "(", values[1], ",", values[-1], ")")
  }
}

lines <- function(...) paste(..., sep = "", collapse = "\n")

