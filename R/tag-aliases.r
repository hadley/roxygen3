#' Add additional topic aliases.
#' 
#' Add additional aliases through which the user can find the documentation
#' with \code{\link{help}}. The topic name is always included in the list of
#' aliases.
#'
#' @usage @@aliases space separated aliases

setClass("TagAliases", contains = "Tag")

setMethod("procTag", "TagAliases", function(tag) {
  tag@text <- words_tag(min = 1)(tag@text)
  tag
})

setMethod("writeRd", "TagAliases", function(tag) {
  new_command("alias", tag@text)
})
