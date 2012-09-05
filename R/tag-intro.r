setClass("TagIntro", contains = "Tag")

#' Title, description and details.
#'
#' @usage The first line becomes the topic title.
#'
#' The next paragraph goes into the descrption section.  Any of these
#' sections can span multiple lines.
#'
#' The third and subsequent paragraph go into the details.
#'
#' @seealso \code{\link{tag_title}}, \code{\link{tag_description}},
#'  \code{\link{tag_details}} to set each component individually.
#' @auto_imports
setMethod("procBlock", "TagIntro", function(tag, block) {
  tags <- block@tags
  paragraphs <- str_trim(str_split(tags$intro, fixed('\n\n'))[[1]])

  if (length(paragraphs) == 0) return(block)

  # 1st paragraph = title (unless has @title)
  if (!is.null(tags$title)) {
    title <- tags$title@text
  } else {
    title <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  }

  # 2nd paragraph = description (unless has @description)
  if (!is.null(tags$description)) {
    description <- tags$title@text
  } else if (length(paragraphs) > 0) {
    description <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    # Description is required, so if missing description, repeat title.
    description <- title
  }

  # Every thing else is details and gets combined with existing 
  
  modify_tags(block,
    intro = NULL,
    title = title,
    description = description,
    details = suffix(paragraphs)
  )
})

base_prereqs[["TagIntro"]] <- c("TagTitle", "TagDescription", "TagDetails")
