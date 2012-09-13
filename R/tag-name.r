#' Override the default topic name.
#'
#' By default, the topic name is derived from the object following the
#' roxygen comments. This tag is rarely needed - if you are using it often
#' it's probably an indication that you need to extend roxygen3 for you
#' object type.
#'
#' @usageTag @@name name
setClass("NameTag", contains = "Tag")

setMethod("process", "NameTag", function(input, block) {
  tag(block, "aliases") <- suffix(input@text)
  block
})

setMethod("defaultTag", c("NameTag", "Object"),
  function(tag, object) {
    new("NameTag", text = object@name)
  }
)

setMethod("defaultTag", c("NameTag", "NullObject"),
  function(tag, object) {
    NULL
  }
)

# http://cran.r-project.org/doc/manuals/R-exts.html#Documenting-S4-classes-and-methods
setMethod("defaultTag", c("NameTag", "S4ClassObject"),
  function(tag, object) {
    new("NameTag", text = str_c(object@name, "-class"))
  }
)
setMethod("defaultTag", c("NameTag", "S4MethodObject"),
  function(tag, object) {
    sig <- str_c(object@value@defined, collapse = ",")
    name <- str_c(object@value@generic, ",", sig, "-method")
    new("NameTag", text = name)
  }
)


setMethod("writeRd", "NameTag", function(object) {
  new_command("name", object@text)
})
