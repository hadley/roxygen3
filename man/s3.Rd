% Built by roxygen3 0.1
\name{is_s3_generic}
\alias{is_s3_generic}
\alias{roxgyen_s3}
\title{[DEV] Determine if a function is an S3 generic or S3 method.}
\usage{
  is_s3_generic(name, env = parent.frame())
}
\arguments{
  \item{name}{name of function.}

  \item{env}{environment to search in.}
}

\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

\code{is_s3_generic} compares name to \code{.knownS3Generics} and
\code{.S3PrimitiveGenerics}, then uses \code{\link[codetools]{findGlobals}}
to see if the functionion calls \code{\link{UseMethod}}.

\code{is_s3_method} builds names of all possible generics for that function
and then checks if any of them actually is a generic.
}
\seealso{
Other s3: \code{\link{add_s3_metadata}}
}
