setClass("UsageTagTag", contains = "Tag")

setMethod("writeRd", "UsageTagTag", function(object) {
  contents <- str_c("\\code{\n#' ", object@text, "\n}")
  new_command("section", c("Tag Usage" = contents))
})
