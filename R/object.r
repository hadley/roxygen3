setMethod("show", "RoxyObject", function(object) {
  message("RoxyObject: ", object@name)
  cat(getSrcFilename(object@srcref), ":", 
    object@srcref[1], ":", object@srcref[2], ":\n", sep = "")
  print(object@srcref)
})

setClass("ObjectNull", contains = "RoxyObject")
setMethod("isNull", "ObjectNull", function(x) TRUE)
