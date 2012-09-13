setClass("ExampleTag", contains = "Tag")
setMethod("procBlock", "ExampleTag", function(tag, block) {
  paths <- str_trim(tag@text)
  examples <- unlist(lapply(paths, readLines))

  tag(block, "examples") <- suffix(examples)
  block
})

setMethod("getPrereqs", "ExampleTag", function(tag) "ExampleTag")
