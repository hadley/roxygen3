% Built by roxygen3 0.1
\name{writeRd}
\alias{writeRd}
\title{[DEV] Output to Rd files in the man directory.}
\usage{
  writeRd(object)
}
\arguments{
  \item{object}{Object to proccess, starting at a
  \linkS4class{Bundle}, breaking down into
  \linkS4class{Block}s then individual \linkS4class{Tag}s}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  This uses the \code{@rdname} tag to determine which file
  the output from each rocblock is sent to.
}
