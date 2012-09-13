
rd_wrap <- function(x, indent = 2, exdent = indent) {
  wrapped <- str_wrap(x, width = 60, indent = indent, exdent = exdent)
  str_c(wrapped, collapse = "\n\n")
}
code_wrap <- function(x, indent = 2, exdent = 4) {
  wrapped <- str_wrap(x, width = 60, indent = indent, exdent = exdent)
  wrapped <- str_replace_all(wrapped, fixed("\u{A0}"), " ")
  str_c(wrapped, collapse = "\n\n")
}

