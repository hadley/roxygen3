setClass("PackageObject", contains = "Object")
setClass("DataObject", contains = "Object")
setClass("FunctionObject", contains = "Object")
setClass("S4ClassObject", contains = "Object")
setClass("S4MethodObject", contains = "Object")
setClass("S4GenericObject", contains = "Object")
setClass("S3MethodObject", contains = "FunctionObject")
setClass("S3GenericObject", contains = "FunctionObject")
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
