\docType{methods}
\name{writeRd}

\alias{}
\alias{writeRd}

\title{[DEV] Output to Rd files in the man directory.}

\usage{
  \S4method{writeRd}{TagAliases}(object)

  \S4method{writeRd}{TagAuthor}(object)

  \S4method{writeRd}{TagComment}(object)

  \S4method{writeRd}{TagDescription}(object)

  \S4method{writeRd}{TagDetails}(object)

  \S4method{writeRd}{TagDocType}(object)

  \S4method{writeRd}{TagExamples}(object)

  \S4method{writeRd}{TagFormat}(object)

  \S4method{writeRd}{TagKeywords}(object)

  \S4method{writeRd}{TagName}(object)

  \S4method{writeRd}{TagNote}(object)

  \S4method{writeRd}{TagParam}(object)

  \S4method{writeRd}{TagPath}(object)

  \S4method{writeRd}{TagReference}(object)

  \S4method{writeRd}{TagReturn}(object)

  \S4method{writeRd}{TagSection}(object)

  \S4method{writeRd}{TagSeealso}(object)

  \S4method{writeRd}{TagSeealso}(object)

  \S4method{writeRd}{TagSource}(object)

  \S4method{writeRd}{TagTitle}(object)

  \S4method{writeRd}{TagUsage}(object)

  \S4method{writeRd}{RoxyPackage}(object)

  \S4method{writeRd}{RoxyBundle}(object)

  \S4method{writeRd}{RoxyBlock}(object)

  \S4method{writeRd}{Tag}(object)
}

\arguments{
  \item{tag}{a function that takes a single argument (tag)
  as input, and returns a list of \code{rd_commands} as
  output}

  \item{name}{input tag name, usually set by
  \code{\link{roccer}}.}
}

\description{
  This function is useful only for developers

  This uses the \code{@rdname} tag to determine which file
  the output from each rocblock is sent to.
}

\details{


  Only one of \code{tag} and \code{out} can be supplied.
}

