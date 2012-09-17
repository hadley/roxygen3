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

setMethod("process", "S3methodTag", function(input, block) {
  entries <- str_split(input@text, "[[:space:]]+")

  directives <- lapply(entries, auto_s3, object = block@object)

  input@methods <- do.call("rbind", directives)
  tag(block, "s3method") <- input
  block
})

auto_s3 <- function(text, object) {
  n <- length(text)
  if (n == 0 || (n == 1 && text == "")) {
    # Empty, so guess from name
    pieces <- s3_method_info(object@value)
    generic <- pieces[1]
    class <- pieces[2]
  } else if (n == 1) {
    # Empty, generic provided
    generic <- text
    class <- str_replace(object@name, fixed(str_c(generic, ".")), "")
  } else if (n == 2) {
    generic <- text[1]
    class <- text[2]
  } else {
    message("Invalid @s3method tag")
  }
  cbind(generic, class)
}

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
