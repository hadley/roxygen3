#' @@rdname: manually set the generated file name.
#'
#' Since roxygen3 automatically translates non-alphanumeric components in the
#' function name, you should only need to use this if you want to merge the
#' documentation from multiple blocks into the same file.
#'
#' @tagUsage @@rdname name-of-file-without-extension.
setClass("RdnameTag", contains = "Tag")
