% Built by roxygen3 0.1
\docType{class}
\name{TemplateTag-class}
\alias{TemplateTag-class}
\alias{TemplateVarTag-class}
\title{Include documentation templates.}
\description{
Templates should be stored in \file{man-roxygen/} with a \file{.r} extension.
Templates are parsed with \code{\link[brew]{brew}}, so you can access
template variables using \code{<\%= name \%>}.
}
\keyword{classes}
\section{Tag Usage}{
\preformatted{
#' @template name_of_template
#'  @templateVar name value
}
}
