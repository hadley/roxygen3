#' @@include: describe files that should be sourced before this one.
#'
#' @details
#' The collation order is only modified if it is different from alphabetical
#' order (in the C locale). That is R's default ordering.
#'
#' @tagUsage @@include file-1.r file-2.r
setClass("IncludeTag", contains = "Tag")
