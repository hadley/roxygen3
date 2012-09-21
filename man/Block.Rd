% Built by roxygen3 0.1
\docType{class}
\name{Block-class}
\alias{Block}
\alias{Block-class}
\title{[DEV] Block class.}
\usage{
  Block(tags = list(), object = new("NullObject"),
    srcref = new("NullSrcref"), default_tags = base_tags())
}
\arguments{
  \item{tags}{a list of \code{\linkS4class{Tag}} objects}

  \item{object}{the documented object, of class
  \code{\linkS4class{Object}}}

  \item{srcref}{a srcref giving the location of the block}

  \item{default_tags}{a character vector giving the names
  of the classes that should be used to add default tags to
  the class.}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  The block class encapsulates the data about a single
  roxygen comment block, along with its location in the src
  code, and the object associated with the block.
}
\keyword{classes}
