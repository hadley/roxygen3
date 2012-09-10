setClass("PackageObject", contains = "RoxyObject")
setClass("DataObject", contains = "RoxyObject")
setClass("FunctionObject", contains = "RoxyObject")
setClass("S4ClassObject", contains = "RoxyObject")
setClass("S4MethodObject", contains = "RoxyObject")
setClass("S4GenericObject", contains = "RoxyObject")
setClass("S3MethodObject", contains = "FunctionObject")
setClass("S3GenericObject", contains = "FunctionObject")
setClass("R5ClassObject", contains = "S4ClassObject")

setMethod("show", "RoxyObject", function(object) {
  message("RoxyObject: ", object@name)
  cat(location(object@srcref), sep = "")
  if (!isNull(object@srcref)) {
    print(object@srcref)
  } else {
    cat("\n")
  }
})

setClass("ObjectNull", contains = "RoxyObject")
setMethod("isNull", "ObjectNull", function(x) TRUE)
