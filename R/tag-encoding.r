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
