% Built by roxygen3 0.1
\docType{class}
\name{SlotTag-class}
\alias{AutoSlotsTag-class}
\alias{SlotTag-class}
\title{@slot: document the slots of a S4 class.}
\description{
  Many S4 slots are considered internal implementation
  details, so slots (unlike params) are not documented by
  default. You can use the \code{@slot} and
  \code{@autoSlots} tags to document them. An example of
  the output is include below.
}
\details{
  If a name but no description is provided for
  \code{@slot}, the description will just contain a link to
  the documentation for the type of object that goes in
  that slot.  This is what is used for all slots if you use
  \code{@autoSlots}.
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @slot name
#'  @slot name description
#'  @autoSlots
}
}
\section{Slots}{
\describe{
  \item{slots}{An object of class \linkS4class{character}}

  \item{classname}{An object of class \linkS4class{character}}

  \item{text}{An object of class \linkS4class{character}}

  \item{srcref}{An S3 object of class srcref}

}
}
