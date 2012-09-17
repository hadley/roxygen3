#' Export S3 methods.
#'
#' @usageTag
#'   @@S3method generic class
#'   @@S3method generic
#'   @@S3method
setClass("S3methodTag", contains = "Tag",
  list("methods" = "matrix"))

setMethod("value", "S3methodTag", function(tag) {
  tag@methods
})

setMethod("value<-", "S3methodTag", function(tag, value) {
  tag@text <- parse_words(tag, value, 0, 2)
  tag
})
setMethod("process", "S3methodTag", function(input, block) {
  n <- length(input@text)

  if (n == 0) {
    # Empty, so guess from name
    pieces <- s3_method_info(block@object@value)
    generic <- pieces[1]
    class <- pieces[2]
  } else if (n == 1) {
    # Empty, generic provided
    generic <- input@text
    class <- str_replace(block@object@name, fixed(str_c(generic, ".")), "")
  } else {
    generic <- input@text[1]
    class <- input@text[2]
  }

  input@methods <- cbind(generic, class)
  input@text <- character()

  tag(block, "s3method") <- input
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
