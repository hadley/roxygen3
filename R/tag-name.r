#' Override the default topic name.
#' 
#' By default, the topic name is derived from the object following the 
#' roxygen comments. This tag is rarely needed - if you are using it often
#' it's probably an indication that you need to extend roxygen3 for you 
#' object type.
#'
#' @tagUsage @@name name
setClass("TagName", contains = "Tag")

setMethod("procBlock", "TagName", function(tag, block) {
  modify_tags(block,
    aliases = suffix(tag@text))
})

setMethod("defaultTag", c("TagName", "Object"),
  function(tag, object) {
    new("TagName", text = object@name)
  }
)

# http://cran.r-project.org/doc/manuals/R-exts.html#Documenting-S4-classes-and-methods
setMethod("defaultTag", c("TagName", "S4ClassObject"),
  function(tag, object) {
    new("TagName", text = str_c(object@name, "-class"))
  }
)
setMethod("defaultTag", c("TagName", "S4MethodObject"),
  function(tag, object) {
    sig <- str_c(object@value@defined, collapse = ",")
    name <- str_c(object@value@generic, ",", sig, "-method")
    new("TagName", text = name)
  }
)


setMethod("writeRd", "TagName", function(object) {
  new_command("name", object@text)
})