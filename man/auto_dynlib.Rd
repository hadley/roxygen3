% Built by roxygen3 0.1
\name{auto_dynlib}
\alias{auto_dynlib}
\title{Automatically determine the dynamic imports that a function needs.}
\usage{
  auto_dynlib(x)
}
\arguments{
  \item{x}{function to inspect}
}

\description{
  It does this by walking the call tree and finding all
  calls to \code{.Call}, \code{.C} and \code{.Fortran).
}
\keyword{internal}
