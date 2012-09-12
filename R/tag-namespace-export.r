
#' @tagUsage @@exportClass class1 class2
#' @rdname tag-export
setClass("TagExportClass", contains = "Tag")
setMethod("procTag", "TagExportClass", function(tag) {
  parse_words(tag)
})
setMethod("writeNamespace", "TagExportClass", function(object) {
  ns_each("exportClass", object@text)
})

#' @tagUsage @@exportMethods generic1 generic2
#' @rdname tag-export
setClass("TagExportMethods", contains = "Tag")
setMethod("procTag", "TagExportMethods", function(tag) {
  parse_words(tag)
})
setMethod("writeNamespace", "TagExportMethods", function(object) {
  ns_each("exportMethods", object@text)
})

#' @tagUsage @@exportPattern pattern
#' @rdname tag-export
setClass("TagExportPattern", contains = "Tag")
setMethod("procTag", "TagExportPattern", function(tag) {
  parse_words(tag)
})
setMethod("writeNamespace", "TagExportPattern", function(object) {
  ns_each("exportPattern", object@text)
})

#' @usage
#'   @@S3method generic class
#'   @@S3method generic
#'   @@S3method
#' @rdname tag-export
setClass("TagS3method", contains = "Tag",
  list("methods" = "matrix"))
setMethod("procTag", "TagS3method", function(tag) {
  parse_words(tag, 0, 2)
})
setMethod("procBlock", "TagS3method", function(tag, block) {
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

setMethod("writeNamespace", "TagS3method", function(object) {
  if (length(object@methods) == 0) return()

  if (is.vector(object@methods)) {
    methods <- matrix(object@methods, ncol = 2)
  } else {
    methods <- object@methods
  }

  str_c("S3method(", quote_if_needed(methods[, 1]), ",",
    quote_if_needed(methods[, 2]), ")", collapse = "\n")
})
