setClass("MethodTag", contains = "Tag")

setMethod("value<-", "MethodTag", function(tag, value) {
  message("@method is deprecated. S3 methods are now detected automatically.")
  tag
})
