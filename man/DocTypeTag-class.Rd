% Built by roxygen3 0.1
\docType{class}
\name{DocTypeTag-class}
\alias{DocTypeTag-class}
\title{@docType: set the documentation type of the object.}
\description{
Roxygen3 will guess the doctype for you - the only time you should need to
specify it yourself for package documentation (\code{@docType package})
which typically document the code \code{NULL}.
}
\details{
You can use any doctype, but it will only be included in
  the Rd file if it is one of the standard R doctypes: data, package,
  methods and class.
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @docType data
#'  @docType package
#'  @docType custom doctype
}
}
