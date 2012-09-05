setClass("TagInheritParams", contains = "Tag")
setMethod("getPrereqs", "TagInheritParams", function(tag) {
  c("tagParam", "tagName")
})
