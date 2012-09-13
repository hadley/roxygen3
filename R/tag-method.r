setClass("MethodTag", contains = "Tag")

setMethod("procTag", "MethodTag", function(tag) {
  message("@method is deprecated. S3 methods are now detected automatically.")
})
