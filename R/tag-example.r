setClass("TagExample", contains = "Tag")
setMethod("procBlock", "TagExample", function(tag, block) {
  paths <- str_trim(tag@text)
  examples <- unlist(lapply(paths, readLines))

  tag(block, "examples") <- suffix(examples)
  block
})

setMethod("getPrereqs", "TagExample", function(tag) "TagExamples")
