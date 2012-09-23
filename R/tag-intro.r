#' Title, description and details.
#'
#' @tagUsage The first line becomes the topic title.
#'
#' The next paragraph goes into the descrption section.  Any of these
#' sections can span multiple lines.
#'
#' The third and subsequent paragraph go into the details.
#'
#' @seealso \code{\linkS4class{TitleTag}}, \code{\linkS4class{DescriptionTag}},
#'  \code{\linkS4class{DetailsTag}} to set each component individually.
#' @autoImports
setClass("IntroTag", contains = "Tag")

setMethod("process", "IntroTag", function(input, block) {
  paragraphs <- str_trim(str_split(input@text, fixed('\n\n'))[[1]])
  tag(block, "intro") <- NULL

  if (length(paragraphs) == 0) return(block)

  # 1st paragraph = title (unless has @title)
  title <- tag_value(block, "title")
  if (is.null(title)) {
    title <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  }
  tag(block, "title") <- title

  # 2nd paragraph = description (unless has @description)
  description <- tag_value(block, "description")
  if (is.null(description)) {
    if (length(paragraphs) > 0) {
      description <- paragraphs[1]
      paragraphs <- paragraphs[-1]
    } else {
      # Description is required, so if missing description, repeat title.
      description <- title
    }
  }
  tag(block, "description") <- description

  # Everything else is details and gets combined with existing
  tag(block, "details") <- suffix(paragraphs)

  block
})

setMethod("getPrereqs", "IntroTag", function(tag) {
  c("TitleTag", "DescriptionTag", "DetailsTag")
})

