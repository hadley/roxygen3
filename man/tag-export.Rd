% Built by roxygen3 0.1
\docType{class}
\name{ExportClassTag-class}
\alias{ExportClassTag-class}
\alias{ExportMethodsTag-class}
\alias{ExportPatternTag-class}
\alias{ExportTag-class}
\title{Namespace: tags for exporting objects}
\description{
Generally, you will only need to use a bare \code{@export} - this will
inspect the documented object and automatically figure out the correct
export (which varies based on whether it's a function, s3 method, s4
class, s4 generic/method).
}
\details{
The convention in \pkg{roxygen3} is that exporting a generic automatically
exports all associated methods. This happens automatically for S4 with
the \code{exportMethod} directive, but needs considerable work for S3.
For details, see \link{roxgyen_s3}.

It's not recommend practice to use \code{@exportPattern} (instead it's
better to individually label the functions to be exported), but this may
be useful for legacy packages.

\code{@S3method} exists largely for compatibility with roxygen2. Roxygen3
now automatically determines if an object is an S3 method, and so only
\code{@export is necessary.}
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @exportClass class1 class2
#' @exportMethods generic1 generic2
#' @exportPattern pattern
#' @export
#'  @export function name
}
}
