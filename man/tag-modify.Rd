% Built by roxygen3 0.1
\name{tag}
\alias{prefix}
\alias{suffix}
\alias{tag}
\alias{tag<-}
\title{[DEV] Convenience methods for manipulating tags in a block.}
\usage{
  tag(block, tag_name, create = TRUE)

  tag(block, tag_name, value) <- value

  suffix(x)

  prefix(x)
}
\arguments{
  \item{block}{\code{\link{Block}} object to modify}

  \item{tag_name}{name of the tag (as a length 1 character vector)}

  \item{create}{if \code{TRUE} will create a new \code{Tag} of the appropriate
  type if one is not present.  If \code{FALSE}, it will return \code{NULL}.}

  \item{value}{the replacement value. If it is a character string, it
  will replace the \code{text} slot. If it's a \code{suffix} object, it
  will be added to the end of the tag's text. If it's a \code{prefix}
  object it will be added to the beginning of the tag's text. Otherwise, the
  existing tag will be replaced with the new value.}

  \item{x}{for \code{suffix} and \code{prefix}, text.}
}

\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

Convenience methods for manipulating tags in a block.
}
\examples{
block <- Block()
tag(block, "name")
tag(block, "name", create = FALSE)

tag(block, "name") <- "myname"
block
tag(block, "aliases") <- tag(block, "name")@text
block
tag(block, "aliases") <- suffix("end")
block
tag(block, "aliases") <- prefix("start")
block
}

\keyword{methods}
