#' Do not produce rd file for this object.
#'
#' Note that this only suppresses Rd output, not namespace or other files.
#' @usage @@noRd
setClass("TagNoRd", contains = "Tag")