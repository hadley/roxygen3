setClass("UsageTagTag", contains = "Tag")

setMethod("writeRd", "UsageTagTag", function(object) {
  contents <- str_c("\\code{\n#' ", object@text, "\n}")
  RdCommand("section", c("Tag Usage" = contents))
})
