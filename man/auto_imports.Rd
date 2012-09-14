% Built by roxygen3 0.1
\name{auto_imports}
\alias{auto_imports}
\title{[DEV] Scans a function and determines what functions it uses from other packages.}
\usage{
  auto_imports(f, name = deparse(substitute(f)),
    imported = NULL, imports = NULL)
}
\arguments{
  \item{f}{function to process}

  \item{name}{name of function. If omitted, defaults to the
  deparsed call to \code{f} - this is only useful for
  interactive use.}

  \item{imported}{list of functions that are already
  imported}

  \item{imports}{list of imported packages. If omitted,
  \code{auto_imports} will attempted to determine
  automatically by inspecting the the \file{DESCRIPTION} of
  the package that \code{f} belongs to.}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  Scans a function and determines what functions it uses
  from other packages.
}
\details{
  If a function is available in two or more packages, then
  it is flagged as a conflict, and it must be resolved by
  hand using \code{@importFrom}. The only exception is if a
  function of that name exists in the same environment as
  f: in that case, no import statement will be generated.

  All packages (including recommend packages like stats and
  graphics) need explicit imports in the \file{NAMESPACE}
  file. The only exception is the base package.
}
