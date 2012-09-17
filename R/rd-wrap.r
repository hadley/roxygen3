#' @autoImports
rd_wrap <- function(x, indent = 2, exdent = indent) {
  lines <- str_split(x, "\n")[[1]]
  if (all(str_length(lines) < 60)) width <- Inf

  wrapped <- str_wrap(x, width = 60, indent = indent, exdent = exdent)
  str_c(wrapped, collapse = "\n\n")
}
code_wrap <- function(x, indent = 2, exdent = 4) {
  wrapped <- str_wrap(x, width = 60, indent = indent, exdent = exdent)
  wrapped <- str_replace_all(wrapped, fixed("\u{A0}"), " ")
  str_c(wrapped, collapse = "\n\n")
}

