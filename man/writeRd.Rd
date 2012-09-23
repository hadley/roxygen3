% Built by roxygen3 0.1
\name{writeRd}
\alias{writeRd}
\title{[DEV] Output to Rd files in the man directory.}
\usage{
  writeRd(object)
}
\arguments{
  \item{object}{Object to proccess, starting at a \linkS4class{Bundle},
  breaking down into \linkS4class{Block}s then individual
  \linkS4class{Tag}s}
}

\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

This uses the \code{@rdname} tag to determine which file the output from
each rocblock is sent to.
}
\section{Method signatures for generic writeRd}{
\itemize{
  \item \linkS4class{AliasesTag}
  \item \linkS4class{AuthorTag}
  \item \linkS4class{Block}
  \item \linkS4class{Bundle}
  \item \linkS4class{CommentTag}
  \item \linkS4class{DebugTag}
  \item \linkS4class{DescriptionTag}
  \item \linkS4class{DetailsTag}
  \item \linkS4class{DocTypeTag}
  \item \linkS4class{ExamplesTag}
  \item \linkS4class{FormatTag}
  \item \linkS4class{KeywordsTag}
  \item \linkS4class{NameTag}
  \item \linkS4class{NoteTag}
  \item \linkS4class{PackageBundle}
  \item \linkS4class{ParamTag}
  \item \linkS4class{ReferencesTag}
  \item \linkS4class{ReturnTag}
  \item \linkS4class{SectionTag}
  \item \linkS4class{SeealsoTag}
  \item \linkS4class{SlotTag}
  \item \linkS4class{SourceTag}
  \item \linkS4class{Tag}
  \item \linkS4class{TagUsageTag}
  \item \linkS4class{TitleTag}
  \item \linkS4class{UsageTag}
}

}

\keyword{methods}
