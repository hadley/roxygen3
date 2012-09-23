#' @@name: Override the default topic name.
#'
#' By default, the topic name is derived from the object following the
#' roxygen comments. This tag is rarely needed and using it may suggest
#' that you are documenting the wrong object.
#'
#' @tagUsage @@name name
#' @seealso @@rdname for modifying the generated file name and combining
#'   the documentation for multiple objects into one file.
setClass("NameTag", contains = "Tag")

setMethod("process", "NameTag", function(input, block) {
  tag(block, "aliases") <- suffix(nice_alias(input@text))
  if (isEmpty(tag(block, "rdname"))) {
    tag(block, "rdname") <- nice_name(input@text)
  }

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
    new("NameTag", text = topicName(object@value))
  }
)
setMethod("defaultTag", c("NameTag", "S4MethodObject"),
  function(tag, object) {
    new("NameTag", text = topicName(object@value))
  }
)

setMethod("writeRd", "NameTag", function(object) {
  RdCommand("name", object@text)
})
