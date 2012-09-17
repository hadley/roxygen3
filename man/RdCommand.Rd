% Built by roxygen3 0.1
\name{RdCommand}
\alias{RdCommand}
\title{[DEV] Translate a command and expressions into an Rd expression;
multiple expressions take their own braces.}
\usage{
  RdCommand(command, values)
}
\arguments{
  \item{command}{name of the command}

  \item{values}{character vector (possibly named)}
}

\description{
  This function is aimed primarily at developers extending
  this package. You shouldn't need to use it for everyday
  operation of the package.

  commands have two methods: \code{merge} and
  \code{format}.  Currently for all commands, merge just
  combines all values, and format selects from these to
  display the commands in the appropriate way.
}
