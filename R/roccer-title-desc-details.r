#' Override default topic title.
#'
#' The topic title. By default this is taken from the first paragraph of the
#' roxygen block. See \code{\link{tag__intro}} for more details.
#'
#' @usage @@title Topic title
add_tag_roccer("title", text_tag())

#' Override default description.
#'
#' The topic title. By default this is taken from the second paragraph of the
#' roxygen block. See \code{\link{tag__intro}} for more details.
#'
#' @usage @@description Text goes here.
add_tag_roccer("description", text_tag())

#' Override default details.
#'
#' The topic title. By default this is taken from the third and subsequent 
#' paragraphs of the roxygen block. See \code{\link{tag__intro}} for more
#' details.
#'
#' @usage @@details Text goes here.
add_tag_roccer("details", text_tag())
