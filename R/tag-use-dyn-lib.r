#' Import routines from a shared library.
#'
#' For more details on how to use, see
#' \url{http://cran.r-project.org/doc/manuals/R-exts.html#Load-hooks}
#'
#' \code{useDynLib} directives of the form
#' \code{myDLL = useDynLib(foo, myRoutine_sym = myRoutine, myOtherRoutine)}
#' are not currently supported.
#'
#' @usageTag
#'   @@useDynLib
#'   @@useDynLib foo
#'   @@useDynLib foo, myRoutine, myOtherRoutine
#'   @@useDynLib foo, myRoutine_sym = myRoutine, myOtherRoutine
#'   @@useDynLib myDLL, .registration = TRUE
#'   @@useDynLib KernSmooth, .registration = TRUE, .fixes = "F_"
setClass("UseDynLibTag", contains = "Tag")

setMethod("process", "UseDynLibTag", function(input, block) {
  if (!isEmpty(input)) return(block)
  tag(block, "useDynLib") <- auto_dynlib(block@object@value)
  block
})

setMethod("writeNamespace", "UseDynLibTag", function(object) {
  str_c("useDynLib(", object@text, ")")
})

#' Automatically determine the dynamic imports that a function needs.
#'
#' It does this by walking the call tree and finding all calls to \code{.Call},
#' \code{.C} and \code{.Fortran}.
#'
#' @keywords internal
#' @param x function to inspect
auto_dynlib <- function(x) {
  stopifnot(is.function(x))

  c_calls <- find_calls(x, quote(.C))
  call_calls <- find_calls(x, quote(.Call))
  fortran_calls <- find_calls(x, quote(.Fortran))
  external_calls <- find_calls(x, quote(.External))

  all_calls <- c(c_calls, call_calls, fortran_calls, external_calls)

  get_name <- function(x) deparse(x[[2]])
  names <- vapply(all_calls, get_name, character(1))
  names <- str_replace_all(names, "\"", "")

  pkg <- getPackageName(environment(x))
  str_c(pkg, ", ", names)
}

find_calls <- function(obj, call) {
  if (is.function(obj)) {
    return(find_calls(body(obj), call))
  }

  # If not a call, must be at a leaf, so we haven't found it and
  # don't need to recurse
  if (!is.call(obj)) return()

  # We've found it so return the complete call
  if (identical(obj[[1]], call)) {
    return(list(obj))
  }

  # Recurse through arguments to the call
  unlist(compact(lapply(as.list(obj[-1]), find_calls, call = call)))
}
