setClass("ParamTag", contains = "Tag",
  list(arguments = "character"))

setMethod("value", "ParamTag", function(tag) tag@arguments)

setMethod("value<-", "ParamTag", function(tag, value) {
  pieces <- str_split_fixed(value, "[[:space:]]+", 2)
  tag@arguments <- setNames(pieces[, 2], pieces[, 1])
  tag
})

setMethod("writeRd", "ParamTag", function(object) {
  RdCommand("arguments", object@arguments)
})
