setClass("TagUsage", contains = "Tag",
  list(usage = "character"))
setMethod("procBlock", "TagUsage", function(tag, block) {
  if (!is.null(tag@text)) {
    usage <- tag@text
  } else {
    obj <- block@object
    usage <- usage(obj@value, obj@name, obj@srcref)
  }
  modify_tags(block, usage = usage)
})
