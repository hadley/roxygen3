setClass("TagParam", contains = "Tag",
  list(arguments = "character"))

setMethod("procTag", "TagParam", function(tag) {
  pieces <- str_split_fixed(tag, "[[:space:]]+", 2)

  name <- pieces[, 1]
  desc <- str_trim(pieces[, 2])

  # if (any(name == "")) {
  #   stop(key, ' requires a name and description')
  # }
  tag@arguments <- setNames(desc, name)
  tag
})

setMethod("writeRd", "TagParam", function(tag) {
  new_command("arguments", tag)
})
