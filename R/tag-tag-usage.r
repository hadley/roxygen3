setClass("UsageTagTag", contains = "Tag")

setMethod("writeRd", "UsageTagTag", function(object) {
  RdCommand("tagUsage", object@text)
})

setClass("TagUsageCommand", contains = "RdCommand")
setMethod("format", "TagUsageCommand", function(x, ...) {
  lines <- unlist(str_split(x@values, "\n"))
  comments <- str_c("#' ", str_trim(lines), collapse = "\n")

  str_c(
    "\\section{Tag Usage}{\n",
    "\\code{\n", comments, "\n}",
    "\n}"
  )
})
