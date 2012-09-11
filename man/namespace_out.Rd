\name{namespace_out}

\alias{namespace_out}

\title{An output generator for the \file{NAMESPACE} file.}

\usage{
  namespace_out(tag, name = NULL)
}

\arguments{
  \item{tag}{function that processes a single tag. It
  should return a character vector of lines to be included
  in the \file{NAMESPACE}.  Duplicates will be
  automatically removed.}

  \item{name}{input tag name, usually set by
  \code{\link{roccer}}.}
}

\description{
  An output generator for the \file{NAMESPACE} file.
}

\details{

}

