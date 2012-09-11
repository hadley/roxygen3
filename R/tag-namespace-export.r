
#' @usage @@exportClass class1 class2
#' @rdname tag-export
setClass("TagExportClass", contains = "Tag")
setMethod("procTag", "TagExportClass", function(tag) {
  parse_words(tag)
})
setMethod("writeNamespace", "TagExportClass", function(object) {
  ns_each("exportClass", object@text)
})

#' @usage @@exportMethods generic1 generic2
#' @rdname tag-export
setClass("TagExportMethods", contains = "Tag")
setMethod("procTag", "TagExportMethods", function(tag) {
  parse_words(tag)
})
setMethod("writeNamespace", "TagExportMethods", function(object) {
  ns_each("exportMethods", object@text)
})

#' @usage @@exportPattern pattern
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
  list("methods" = "character"))
setMethod("procTag", "TagS3method", function(tag) {
  parse_words(tag, 0, 2)
})
setMethod("procBlock", "TagS3method", function(tag, block) {
  s3method <- tag@text
  n <- length(s3method)

  if (n == 0) return()
  if (n == 2) return()

  if (s3method == "") {
    # Empty, so guess from name
    pieces <- s3_method_info(block@obj@value)
    generic <- pieces[1]
    class <- pieces[2]
  } else {
    generic <- s3method
    class <- str_replace(block@obj@name, fixed(str_c(generic, ".")), "")
  }

  modify_tags(block,
    methods = list(methods = cbind(generic, class)))
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
