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
