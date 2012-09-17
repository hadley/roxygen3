#' Inherit parameters from another function.
#'
#' This tag will bring in all documentation for parameters that are
#' undocumented in the current function, but documented in the source
#' function. The source can be a function in the current package,
#' \code{function}, or another package \code{package::function}.
#'
#' @usageTag @@inheritParams source_function
setClass("InheritParamsTag", contains = "Tag")
