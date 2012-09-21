% Built by roxygen3 0.1
\docType{class}
\name{ImportClassesFromTag-class}
\alias{ImportClassesFromTag-class}
\alias{ImportFromTag-class}
\alias{ImportMethodsFromTag-class}
\alias{ImportTag-class}
\title{Namespace: tags for importing functions.}
\description{
  By and large, \code{@autoImports} should be the only
  imports tag that you need to use. It automatically
  generates the necessary \code{importFrom} statements to
  import all external functions used by this function.  See
  \code{\link{auto_imports}} for more implementation
  details.
}
\details{
  If there is a conflict, use \code{tag_importFrom} to
  resolve it. You can do \code{@importFrom base function} -
  this is not normally allowed in the \file{NAMESPACE}, but
  roxygen3 will simply ignore it, but still use it when
  resolving conflicts.

  You must have the packages declared in \code{DESCRIPTION}
  Imports.
}
\keyword{classes}
\section{Tag Usage}{
\code{
#' @importClassesFrom package fun1 fun2
#' @importFrom package function1 function2
#' @importMethodsFrom package fun1 fun2
#' @import package1 package2 package3
}
}
