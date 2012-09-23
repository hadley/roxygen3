% Built by roxygen3 0.1
\docType{methods}
\name{writeDescription}
\alias{writeDescription}
\alias{writeDescription,PackageBundle-method}
\title{[DEV] An output generator for the \file{DESCRIPTION} file.}
\usage{
  writeDescription(object)

  \S4method{writeDescription}{PackageBundle}(object)
}
\arguments{
  \item{object}{Object to proccess, starting at a \linkS4class{Bundle},
  breaking down into \linkS4class{Block}s then individual
  \linkS4class{Tag}s}
}

\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

An output generator for the \file{DESCRIPTION} file.
}
\section{Method signatures for generic writeDescription}{
\itemize{
  \item \linkS4class{Block}
  \item \linkS4class{Bundle}
  \item \linkS4class{CollateTag}
  \item \linkS4class{PackageBundle}
  \item \linkS4class{Tag}
}

}

\keyword{methods}
