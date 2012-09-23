setGeneric("location", function(x) standardGeneric("location"))

setMethod("location", "NullObject", function(x) "Unknown")
setMethod("location", "NullSrcref", function(x) "Unknown")
setMethod("location", "srcref", function(x) {
  path <- getSrcFilename(x)
  str_c(path, ":", x[1], ":", x[3], ":")
})
setMethod("location", "Object", function(x) {
  str_c(x@name, " at ", location(x@srcref))
})
setMethod("location", "Block", function(x) {
  location(x@srcref)
})

at_location <- function(code, object) {
  with_context(code, str_c("at ", location(object)))
}

#' @examples
#' y <- with_context({
#'   print("1")
#'   warning("2")
#'   message("3")
#'   warning("4")
#'   stop("5")
#'   message("6")
#' }, "with context.")
with_context <- function(code, context) {
  loc_msg <- function(x) {
    msg <- str_replace(x$message, "\n$", "")
    message(msg, " ", context)
    invokeRestart("muffleMessage")
  }
  loc_warning <- function(x) {
    msg <- str_replace(x$message, "\n$", "")
    message("Warning message:\n", msg, " ", context)
    invokeRestart("muffleWarning")
  }
  loc_err <- function(x) {
    msg <- str_replace(x$message, "\n$", "")
    message("Error: ", msg, " ", context)
    invokeRestart("abort")
}

  withCallingHandlers(code,
    message = loc_msg, warning = loc_warning, error = loc_err)
}
