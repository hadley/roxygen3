% Built by roxygen3 0.1
\docType{class}
\name{ClassMethodsTag-class}
\alias{ClassMethodsTag-class}
\title{@classMethods: automatically list all methods of an S4 class.}
\description{
This lists all methods connected to this class, directly, not through
any of its superclasses.
}
\details{
In S4 methods are not associated with classes, but with generic functions.
This tag works by looking at all methods for generic function, and then
only showing those that use this class in at least one element of the
signature.
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @classMethods
}
}
