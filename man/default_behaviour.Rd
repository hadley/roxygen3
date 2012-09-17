% Built by roxygen3 0.1
\name{default_behaviour}
\alias{default_behaviour}
\title{[DEV] Encapuslate default behaviour.}
\usage{
  default_behaviour(debug = FALSE)
}
\arguments{
  \item{debug}{if \code{TRUE}, will automatically add
  source information to add generated Rd files.}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  Roxygen defaults to using all subclasses of
  \code{\linkS4class{Tag}}, and all processors
  (\code{$process[A-Z].*}) and writers
  (\code{$writer[A-Z].*}) defined in the roxygen package.
}
