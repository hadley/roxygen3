
#' @usageTag @@exportClass class1 class2
#' @rdname tag-export
setClass("ExportClassTag", contains = "Tag")
setMethod("value<-", "ExportClassTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})
setMethod("writeNamespace", "ExportClassTag", function(object) {
  ns_each("exportClass", object@text)
})

#' @usageTag @@exportMethods generic1 generic2
#' @rdname tag-export
setClass("ExportMethodsTag", contains = "Tag")
setMethod("value<-", "ExportMethodsTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})
setMethod("writeNamespace", "ExportMethodsTag", function(object) {
  ns_each("exportMethods", object@text)
})

#' @usageTag @@exportPattern pattern
#' @rdname tag-export
setClass("ExportPatternTag", contains = "Tag")
setMethod("value<-", "ExportPatternTag", function(tag, value) {
  tag@text <- parse_words(tag, value)
  tag
})
setMethod("writeNamespace", "ExportPatternTag", function(object) {
  ns_each("exportPattern", object@text)
})

#' @usage
#'   @@S3method generic class
#'   @@S3method generic
#'   @@S3method
#' @rdname tag-export
setClass("S3methodTag", contains = "Tag",
  list("methods" = "matrix"))
setMethod("value<-", "S3methodTag", function(tag, value) {
  tag@text <- parse_words(tag, value, 0, 2)
  tag
})
setMethod("procBlock", "S3methodTag", function(tag, block) {
  n <- length(tag@text)

  if (n == 0) {
    # Empty, so guess from name
    pieces <- s3_method_info(block@object@value)
    generic <- pieces[1]
    class <- pieces[2]
  } else if (n == 1) {
    # Empty, generic provided
    generic <- tag@text
    class <- str_replace(block@object@name, fixed(str_c(generic, ".")), "")
  } else {
    generic <- tag@text[1]
    class <- tag@text[2]
  }

  tag@methods <- cbind(generic, class)
  tag(block, "s3method") <- tag
  block
})

setMethod("writeNamespace", "S3methodTag", function(object) {
  if (length(object@methods) == 0) return()

  if (is.vector(object@methods)) {
    methods <- matrix(object@methods, ncol = 2)
  } else {
    methods <- object@methods
  }

  str_c("S3method(", quote_if_needed(methods[, 1]), ",",
    quote_if_needed(methods[, 2]), ")", collapse = "\n")
})
