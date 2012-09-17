% Built by roxygen3 0.1
\docType{class}
\name{AutoImportsTag-class}
\alias{AutoImportsTag-class}
\title{Automatically add import statements.}
\description{
  Using this tag will automatically add all external
  functions used by the function to the namespace.  If
  there are any ambiguities (i.e. the same name is used by
  multiple packages) you can resolve them using
  \code{@importFrom}
}
\section{Tag Usage}{
\code{
#' @autoImports
}
}
