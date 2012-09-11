setClass("TagDefaultExport", contains = "Tag", list( 
  exports = "character", 
  exportMethods = "character", 
  exportClass = "character",
  S3method = "character"
))

setMethod("defaultTag", c("TagDefaultExport", "FunctionObject"), 
  function(tag, object) {
    new("TagDefaultExport", exports = object@name)
  }
)
setMethod("defaultTag", c("TagDefaultExport", "S4MethodObject"), 
  function(tag, object) {
    gen <- as.vector(object@value@generic) 
    new("TagDefaultExport", exportMethods = gen)    
  }
)
setMethod("defaultTag", c("TagDefaultExport", "S3MethodObject"), 
  function(tag, object) {
    new("TagDefaultExport", S3method = s3_method_info(obj))
  }
)
setMethod("defaultTag", c("TagDefaultExport", "S3GenericObject"), 
  function(tag, object) {
    all <- all_s3_methods(environment(obj))
    matching <- all[all[, 1] == name, ]

    new("TagDefaultExport", S3method = matching, export = name)
  }
)
setMethod("defaultTag", c("TagDefaultExport", "S4ClassObject"), 
  function(tag, object) {
    new("TagDefaultExport", exportClass = as.vector(obj@className))
  }
)

