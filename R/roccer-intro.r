#' @roc_usage @@title Topic title
#' @roc_desc The topic title. By default this is taken from the first sentence
#'  of the roxygen block. See \code{\link{roc_intro}} for more details.
roc_title <- basic_roccer("@title", sentence_tag())
roc_description <- basic_roccer("@description", paragraph_tag())
roc_details <- basic_roccer("@details", paragraph_tag())

parse_intro <- function(roc, obj) {
  if (!is.null(roc$`_intro`)) {
    paragraphs <- str_trim(str_split(intro, fixed('\n\n'))[[1]])
  } else {
    paragraphs <- NULL
  }

  # 1st paragraph = title (unless has @title)
  if (!is.null(roc$title)) {
    title <- roc$title
  } else if (length(paragraphs) > 0) {
    title <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    title <- NULL
  }

  # 2nd paragraph = description (unless has @description)
  if (!is.null(roc$description)) {
    description <- roc$description
  } else if (length(paragraphs) > 0) {
    description <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    # Description is required, so if missing description, repeat title.
    description <- title
  }

  # Every thing else = details, combined with @details.
  details <- c(paragraphs, roc$details)
  if (length(details) > 0) {
    details <- paste(details, collapse = "\n\n")
  } else {
    details <- NULL
  }
  
  list(
    `_intro` = NULL, 
    title = title, 
    description = description, 
    details = details
  )
}

roc_intro <- roccer("@_intro", roc_parser(one = parse_intro))
base_prereqs[["@_intro"]] <- c("@title", "@description", "@details")
