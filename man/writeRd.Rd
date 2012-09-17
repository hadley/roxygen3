% Built by roxygen3 0.1
\name{writeRd}
\alias{writeRd}
\title{[DEV] Output to Rd files in the man directory.}
\usage{
  writeRd(object)
}
\arguments{
  \item{tag}{a function that takes a single argument (tag)
  as input, and returns a list of \code{rd_commands} as
  output}

  \item{name}{input tag name, usually set by
  \code{\link{roccer}}.}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  This uses the \code{@rdname} tag to determine which file
  the output from each rocblock is sent to.
}
\details{
  Only one of \code{tag} and \code{out} can be supplied.
}
