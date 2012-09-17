% Built by roxygen3 0.1
\docType{class}
\name{UseDynLibTag-class}
\alias{UseDynLibTag-class}
\title{Import routines from a shared library.}
\description{
  Most of the time you should only need to use
  \code{@useDynLib} without any arguments -
  \code{auto_dynlib} will parse the function and extract
  any calls to \code{\link{.C}}, \code{\link{.Fortran}},
  \code{\link{.Call}} or \code{\link{.External}}.
}
\details{
  For more details on how to use, see
  \url{http://cran.r-project.org/doc/manuals/R-exts.html#Load-hooks}

  \code{useDynLib} directives of the form \code{myDLL =
  useDynLib(foo, myRoutine_sym = myRoutine,
  myOtherRoutine)} are not currently supported.
}
\section{Tag Usage}{
  \code{ #' @useDynLib @useDynLib foo @useDynLib foo,
  myRoutine, myOtherRoutine @useDynLib foo, myRoutine_sym =
  myRoutine, myOtherRoutine @useDynLib myDLL, .registration
  = TRUE @useDynLib KernSmooth, .registration = TRUE,
  .fixes = "F_" }
}

