% Built by roxygen3 0.1
\docType{class}
\name{DirectoryBundle-class}
\alias{DirectoryBundle}
\alias{DirectoryBundle-class}
\title{[DEV] A bundle of files in a directory.}
\usage{
  DirectoryBundle(path, behaviour = default_behaviour())
}
\arguments{
  \item{path}{location of folder containing R files}

  \item{behaviour}{\code{\linkS4class{Behaviour}} object describing what tags,
  processors and writers to use}
}

\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

A bundle of files in a directory.
}
\section{Generics with methods for DirectoryBundle}{
\itemize{
  \item \code{\link{rPath}} (1 methods defined for subclasses)
  \item \code{\link{writeDescription}}* (1 methods defined for subclasses)
  \item \code{\link{writeNamespace}}* (1 methods defined for subclasses)
  \item \code{\link{writeRd}}* (1 methods defined for subclasses)
}

* = methods only defined for subclasses
}

\keyword{classes}
