setClass("TagTagUsage", contains = "Tag")

setMethod("writeRd", "TagTagUsage", function(object) {
  contents <- str_c("\\code{\n#' ", object@text, "\n}")
  new_command("section", c("Tag Usage" = contents))
})