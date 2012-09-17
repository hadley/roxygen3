% Built by roxygen3 0.1
\docType{class}
\name{PackageBundle-class}
\alias{PackageBundle}
\alias{PackageBundle-class}
\title{[DEV] A bundle of files in a package.}
\usage{
  PackageBundle(path, behaviour = default_behaviour())
}
\arguments{
  \item{path}{path of package root directory}

  \item{behaviour}{\code{\linkS4class{Behaviour}} object
  describing what tags, processors and writers to use}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  The package class captures all the information about the
  package: its name, path, and all the
  \code{\linkS4class{Block}}s that it contains.
}
