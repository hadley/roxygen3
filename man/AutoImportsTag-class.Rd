% Built by roxygen3 0.1
\docType{class}
\name{AutoImportsTag-class}
\alias{AutoImportsTag-class}
\title{@autoImport: automatically add import statements.}
\description{
  Using this tag will automatically add all external
  functions used by the function to the namespace.  If
  there are any ambiguities (i.e. the same name is used by
  multiple packages) you can resolve them using
  \code{@importFrom}
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @autoImports
}
}
