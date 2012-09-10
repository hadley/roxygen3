#' Override the default topic name.
#' 
#' By default, the topic name is derived from the object following the 
#' roxygen comments. This tag is rarely needed - if you are using it often
#' it's probably an indication that you need to extend roxygen3 for you 
#' object type.
#'
#' @usage @@name name
setClass("TagName", contains = "Tag")

setMethod("procBlock", "TagName", function(tag, block) {
  modify_tags(block,
    aliases = suffix(tag@text))
})

setMethod("writeRd", "TagName", function(output) {
  new_command("name", output@text)
})