#' @@example/@@examples: provide examples.
#'
#' These two tags allow you to add examples to the documentation, either inline
#' with \code{@@examples} or by pointing to an external file with
#' \code{@@example}.
#'
#' @section Escaping rules:
#' As with all roxygen blocks, latex comments, \code{%} are always escaped to
#' \code{\\%}. Backslash behaviour, code{\\}, is special for \code{@@example}
#' and \code{@@examples}. Backslahes will be escaped to \code{\\\\} unless they
#' are for the special macros (the only macros that can be used inside an
#' example block): \code{\\dontrun}, \code{\\donttest}, \code{\\dontrun} or
#' \code{\\testonly}.
#'
#' @tagUsage
#'   @@examples
#'     a <- 1
#'     b <- 2
#'   @@example path/to/example file
#' @rdname examples
#' @examples
#' x <- 1:10
#' a <- "\\link{mean}(x)"
#' a <- "before tab\tafter tab"
#'
#' test <- "\\a\\b\\c\\d\\e\\f\\g\\h\\i\\j\\k\\l\\m\\n\\o\\p\\q\\r\\s\\t\\u\\v\\w\\x\\y\\z"
#' stopifnot(test == paste("\\", letters, collapse = "", sep = ""))
#'
#' x %% 2
#' x %/% 2
#'
#' # A strangely named function
#' "%\\%" <- function(a, b) a + b
#' 10 %\% 10
#'
#' \dontrun{
#'   a <- 1
#'   # this code will not be run
#' }
#' \donttest{
#'   b <- 1
#'   # this code will with example(), but not by R CMD check
#' }
#' \dontshow{
#'   print("What code made me?")
#'   # this code will be run, but not shown
#' }
setClass("ExamplesTag", contains = "Tag")
setMethod("writeRd", "ExamplesTag", function(object) {
  RdCommand("examples", object@text)
})

#' @rdname examples
setClass("ExampleTag", contains = "Tag")
setMethod("process", "ExampleTag", function(input, block) {
  paths <- str_trim(input@text)
  examples <- unlist(lapply(paths, readLines))

  tag(block, "examples") <- suffix(examples)
  block
})

setMethod("getPrereqs", "ExampleTag", function(tag) "ExampleTag")

escape_examples <- function(x) {
  # Escape backslashes
  x <- str_replace_all(x, fixed("\\"), "\\\\")

  macros <- c("dontrun", "donttest", "dontshow", "testonly")
  match <- str_c("\\\\\\\\(", str_c(macros, collapse = "|"), ")")

  str_replace_all(x, match, "\\\\\\1")
}
