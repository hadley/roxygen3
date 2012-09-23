% Built by roxygen3 0.1
\name{parse_file}
\alias{parse_file}
\title{Parse a source file containing roxygen blocks.}
\usage{
  parse_file(path, env, default_tags = base_tags())
}
\arguments{
  \item{path}{path of file to parse}

  \item{env}{environment that contains the results of evaluating the file.}
}

\value{
A list of roc objects
}
\description{
Parse a source file containing roxygen blocks.
}
\keyword{internal}
