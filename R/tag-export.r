#' Namespace: tags for exporting objects
#'
#' Generally, you will only need to use a bare \code{@@export} - this will
#' inspect the documented object and automatically figure out the correct
#' export (which varies based on whether it's a function, s3 method, s4
#' class, s4 generic/method).
#'
#' The convention in \pkg{roxygen3} is that exporting a generic automatically
#' exports all associated methods. This happens automatically for S4 with
#' the \code{exportMethod} directive, but needs considerable work for S3.
#' For details, see \link{roxgyen_s3}.
#' 
#' It's not recommend practice to use \code{@@exportPattern} (instead it's
#' better to individually label the functions to be exported), but this may 
#' be useful for legacy packages.
#'
#' \code{@@S3method} exists largely for compatibility with roxygen2. Roxygen3
#' now automatically determines if an object is an S3 method, and so only
#' \code{@@export is necessary.}
#'
#' @usage 
#'   @@export 
#'   @@export function name
#' @rdname tag-export
setClass("TagExport", contains = "Tag")

setMethod("procBlock", "TagExport", function(tag, block) {
  defaults <- block@tags$defaultExport
  if (is.null(defaults)) return(block)
  
  modify_tags(block,
    export = suffix(defaults@export),
    exportMethods = suffix(defaults@exportMethods),
    exportClass = suffix(defaults@exportClass),
    S3method = new("TagS3method", methods = as.character(defaults@S3method))
  )
})

setMethod("writeNamespace", "TagExport", function(object) {
  ns_each("export", object@text)
})

setMethod("getPrereqs", "TagExport", function(tag) {
  c("TagS3method", "TagDocType")
})
