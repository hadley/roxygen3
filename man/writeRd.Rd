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
  \item \linkS4class{AliasesTag} (S4)
  \item \linkS4class{AuthorTag} (S4)
  \item \linkS4class{Block} (S4)
  \item \linkS4class{Bundle} (S4)
  \item \linkS4class{CommentTag} (S4)
  \item \linkS4class{DebugTag} (S4)
  \item \linkS4class{DescriptionTag} (S4)
  \item \linkS4class{DetailsTag} (S4)
  \item \linkS4class{DocTypeTag} (S4)
  \item \linkS4class{ExamplesTag} (S4)
  \item \linkS4class{FormatTag} (S4)
  \item \linkS4class{KeywordsTag} (S4)
  \item \linkS4class{NameTag} (S4)
  \item \linkS4class{NoteTag} (S4)
  \item \linkS4class{PackageBundle} (S4)
  \item \linkS4class{ParamTag} (S4)
  \item \linkS4class{ReferencesTag} (S4)
  \item \linkS4class{ReturnTag} (S4)
  \item \linkS4class{SectionTag} (S4)
  \item \linkS4class{SeealsoTag} (S4)
  \item \linkS4class{SlotTag} (S4)
  \item \linkS4class{SourceTag} (S4)
  \item \linkS4class{Tag} (S4)
  \item \linkS4class{TagUsageTag} (S4)
  \item \linkS4class{TitleTag} (S4)
  \item \linkS4class{UsageTag} (S4)
}

}

\keyword{methods}
