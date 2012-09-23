#' @rdname Object
setClass("PackageObject", contains = "Object")
#' @rdname Object
setClass("DataObject", contains = "Object")
#' @rdname Object
setClass("FunctionObject", contains = "Object")
#' @rdname Object
setClass("S4ClassObject", contains = "Object")
#' @rdname Object
setClass("S4MethodObject", contains = "Object")
#' @rdname Object
setClass("S4GenericObject", contains = "Object")
#' @rdname Object
setClass("S3MethodObject", contains = "FunctionObject")
#' @rdname Object
setClass("S3GenericObject", contains = "FunctionObject")
#' @rdname Object
setClass("R5ClassObject", contains = "S4ClassObject")

setMethod("show", "Object", function(object) {
  message("Object: ", object@name)
  cat(location(object@srcref), sep = "")
  if (!isNull(object@srcref)) {
    print(object@srcref)
  } else {
    cat("\n")
  }
})

setClass("NullObject", contains = "Object")
setMethod("isNull", "NullObject", function(x) TRUE)
