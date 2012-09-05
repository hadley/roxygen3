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
  name <- tag@text %||% block@obj@name
  if (is.null(name)) {
    message("Missing name. This object will not be documented")
    return()
  }

  modify_tags(block,
    name = name,
    aliases = suffix(name)
  )
})
base_prereqs[["name"]] <- "aliases"