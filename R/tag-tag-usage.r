setClass("UsageTagTag", contains = "Tag")

setMethod("writeRd", "UsageTagTag", function(object) {
  RdCommand("tagUsage", object@text)
})

setClass("TagUsageCommand", contains = "RdCommand")
setMethod("format", "TagUsageCommand", function(x, ...) {
  # First line looses original spacing, so add it back on
  values <- str_c(" ", x@values)
  lines <- unlist(str_split(values, "\n"))
  comments <- str_c("#'", lines, collapse = "\n")

  str_c(
    "\\section{Tag Usage}{\n",
    "\\preformatted{\n", comments, "\n}",
    "\n}"
  )
})
