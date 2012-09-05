setClass("TagUsage", contains = "Tag",
  list(usage = "character"))
setMethod("procBlock", "TagUsage", function(tag, block) {
  if (!is.null(tag@text)) {
    tag@usage <- tag@text
  } else {
    obj <- block@object
    tag@usage <- usage(obj@value, obj@name, obj@srcref)
  }
  tag
})
