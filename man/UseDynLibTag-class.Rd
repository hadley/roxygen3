% Built by roxygen3 0.1
\docType{class}
\name{UseDynLibTag-class}
\alias{UseDynLibTag-class}
\title{Import routines from a shared library.}
\description{
  For more details on how to use, see
  \url{http://cran.r-project.org/doc/manuals/R-exts.html#Load-hooks}
}
\details{
  \code{useDynLib} directives of the form \code{myDLL =
  useDynLib(foo, myRoutine_sym = myRoutine, myOtherRoutine)
  are not currently supported.
}
\section{Tag Usage}{
  \code{ #' @useDynLib @useDynLib foo @useDynLib foo,
  myRoutine, myOtherRoutine @useDynLib foo, myRoutine_sym =
  myRoutine, myOtherRoutine @useDynLib myDLL, .registration
  = TRUE @useDynLib KernSmooth, .registration = TRUE,
  .fixes = "F_" }
}

