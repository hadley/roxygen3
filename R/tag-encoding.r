#' @@encoding: specify non-standard encoding for Rd file
#'
#' Generally you are safest to stick with ASCII encoding - if you need to
#' provide documentation in another language, use this tag to specify how the
#' text is encoded. The safest encodings to use are latin1, latin2 and UTF-8.
#'
#' @tagUsage @@encoding UTF-8
setClass("EncodingTag", contains = "Tag")
setMethod("value<-", "EncodingTag", function(tag, value) {
  words <- unlist(str_split(value, "[[:space:]]+"))
  if (length(words) > 1) {
    message("Can only specify one encoding. Using first.", location(tag))
    words <- words[1]
  }

  tag@text <- words
  tag
})
