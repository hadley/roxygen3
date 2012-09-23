% Built by roxygen3 0.1
\docType{class}
\name{InheritParamsTag-class}
\alias{InheritParamsTag-class}
\title{@inheritParams: Inherit parameters from another function.}
\description{
This tag will bring in all documentation for parameters that are
undocumented in the current function, but documented in the source
function. The source can be a function in the current package,
\code{function}, or another package \code{package::function}.
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @inheritParams source_function
}
}
