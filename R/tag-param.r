setClass("ParamTag", contains = "Tag",
  list(arguments = "character"))

setMethod("value", "ParamTag", function(tag) tag@arguments)

setMethod("value<-", "ParamTag", function(tag, value) {
  pieces <- str_split_fixed(value, "[[:space:]]+", 2)

  name <- pieces[, 1]
  desc <- str_trim(pieces[, 2])

  # if (any(name == "")) {
  #   stop(key, ' requires a name and description')
  # }
  tag@arguments <- setNames(desc, name)
  tag
})

setMethod("writeRd", "ParamTag", function(object) {
  new_command("arguments", object@arguments)
})
