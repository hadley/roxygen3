setClass("UsageTag", contains = "Tag",
  list(usage = "Usage"),
  prototype = list(usage = new("NullUsage")))

setMethod("value", "UsageTag", function(tag) tag@usage)
setMethod("value<-", "UsageTag", function(tag, value) {
  if (is.character(value)) {
    value <- new("TextUsage", text = value)
  }
  tag@usage <- value
  tag
})

setMethod("writeRd", "UsageTag", function(object) {
  RdCommand("usage", format(object@usage))
})

setMethod("defaultTag", c("UsageTag", "FunctionObject"),
  function(tag, object) {
    usage <- new("FunctionUsage",
      name = object@name,
      formals = as.list(formals(object@value)))
    new("UsageTag", usage = usage)
  }
)
setMethod("defaultTag", c("UsageTag", "S3MethodObject"),
  function(tag, object) {
    method <- s3_method_info(object@value)

    usage <- new("S3MethodUsage",
      generic = method[1],
      signature = method[2],
      formals = as.list(formals(object@value)))
    new("UsageTag", usage = usage)
  }
)
setMethod("defaultTag", c("UsageTag", "S4MethodObject"),
  function(tag, object) {
    obj <- object@value
    usage <- new("S4MethodUsage",
      generic = obj@generic,
      signature = obj@defined,
      formals = as.list(formals(obj@.Data)))
    new("UsageTag", usage = usage)
  }
)

setMethod("defaultTag", c("UsageTag", "S4GenericObject"),
  function(tag, object) {
    usage <- new("FunctionUsage",
      name = object@name,
      formals = as.list(formals(object@value@.Data)))
    new("UsageTag", usage = usage)
  }
)

setMethod("defaultTag", c("UsageTag", "DataObject"),
  function(tag, object) {
    usage <- new("TextUsage", text = object@name)
    new("UsageTag", usage = usage)
  }
)

