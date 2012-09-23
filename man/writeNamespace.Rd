% Built by roxygen3 0.1
\name{writeNamespace}
\alias{writeNamespace}
\title{[DEV] An output generator for the \file{NAMESPACE} file.}
\usage{
  writeNamespace(object)
}
\arguments{
  \item{object}{Object to proccess, starting at a \linkS4class{Bundle},
  breaking down into \linkS4class{Block}s then individual
  \linkS4class{Tag}s}
}

\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

An output generator for the \file{NAMESPACE} file.
}
\section{Method signatures for generic writeNamespace}{
\itemize{
  \item \linkS4class{Block}
  \item \linkS4class{Bundle}
  \item \linkS4class{ExportClassTag}
  \item \linkS4class{ExportMethodsTag}
  \item \linkS4class{ExportPatternTag}
  \item \linkS4class{ExportTag}
  \item \linkS4class{ImportClassesFromTag}
  \item \linkS4class{ImportFromTag}
  \item \linkS4class{ImportMethodsFromTag}
  \item \linkS4class{ImportTag}
  \item \linkS4class{PackageBundle}
  \item \linkS4class{S3methodTag}
  \item \linkS4class{Tag}
  \item \linkS4class{UseDynLibTag}
}

}

\keyword{methods}
