% Built by roxygen3 0.1
\name{writeNamespace}
\alias{writeNamespace}
\title{[DEV] An output generator for the \file{NAMESPACE} file.}
\usage{
  writeNamespace(object)
}
\arguments{
  \item{tag}{function that processes a single tag. It
  should return a character vector of lines to be included
  in the \file{NAMESPACE}.  Duplicates will be
  automatically removed.}

  \item{name}{input tag name, usually set by
  \code{\link{roccer}}.}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  An output generator for the \file{NAMESPACE} file.
}
