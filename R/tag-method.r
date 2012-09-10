setClass("TagMethod", contains = "Tag")

setMethod("procTag", "TagMethod", function(tag) {
  message("@method is deprecated. S3 methods are now detected automatically.")
})
