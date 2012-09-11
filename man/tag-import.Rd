\docType{class}
\name{TagImportFrom}

\alias{}
\alias{TagAutoImports}
\alias{TagImport}
\alias{TagImportClassesFrom}
\alias{TagImportFrom}
\alias{TagImportMethodsFrom}

\title{Namespace: tags for importing functions.}

\usage{

}

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

