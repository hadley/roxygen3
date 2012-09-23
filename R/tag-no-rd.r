#' Do not produce rd file for this object.
#'
#' Note that this only suppresses Rd output, not namespace or other files.
#' @tagUsage @@noRd
setClass("NoRdTag", contains = "Tag")
