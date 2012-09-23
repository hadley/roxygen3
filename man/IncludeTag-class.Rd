% Built by roxygen3 0.1
\docType{class}
\name{CollateTag-class}
\alias{CollateTag-class}
\alias{IncludeTag-class}
\title{@include: describe files that should be sourced before this one.}
\description{
@include: describe files that should be sourced before this one.
}
\details{
The collation order is only modified if it is different from alphabetical
order (in the C locale). That is R's default ordering.
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @include file-1.r file-2.r
}
}
