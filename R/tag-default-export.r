setClass("DefaultExportTag", contains = "Tag", list(
  export = "character",
  exportMethods = "character",
  exportClass = "character",
  S3method = "matrix"
), prototype = list(
  export = character(0),
  exportMethods = character(0),
  exportClass = character(0),
  S3method = matrix(character(), ncol = 2)
))

setMethod("isEmpty", "DefaultExportTag", function(tag) {
  length(tag@export) == 0 &&
    length(tag@exportMethods) == 0 &&
    length(tag@exportClass) == 0 &&
    nrow(tag@S3method) == 0
})

setMethod("defaultTag", c("DefaultExportTag", "FunctionObject"),
  function(tag, object) {
    new("DefaultExportTag", export = object@name)
  }
)
setMethod("defaultTag", c("DefaultExportTag", "S4GenericObject"),
  function(tag, object) {
    new("DefaultExportTag", export = object@name, exportMethods = object@name)
  }
)
setMethod("defaultTag", c("DefaultExportTag", "S4MethodObject"),
  function(tag, object) {
    gen <- as.vector(object@value@generic)
    new("DefaultExportTag", exportMethods = gen)
  }
)
setMethod("defaultTag", c("DefaultExportTag", "S3MethodObject"),
  function(tag, object) {
    s3 <- s3_method_info(object@value)
    new("DefaultExportTag", S3method = matrix(s3, ncol = 2))
  }
)
setMethod("defaultTag", c("DefaultExportTag", "S3GenericObject"),
  function(tag, object) {
    all <- all_s3_methods(environment(object@value))
    if (is.null(all)) return()
    matching <- all[all[, 1] == object@name, , drop = FALSE]

    new("DefaultExportTag", S3method = matching, export = object@name)
  }
)
setMethod("defaultTag", c("DefaultExportTag", "S4ClassObject"),
  function(tag, object) {
    new("DefaultExportTag", exportClass = as.vector(object@value@className))
  }
)
setMethod("defaultTag", c("DefaultExportTag", "R5ClassObject"),
  function(tag, object) {
    new("DefaultExportTag", exportClass = as.vector(object@value$className))
  }
)

