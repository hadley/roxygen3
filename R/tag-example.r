setClass("ExampleTag", contains = "Tag")
setMethod("process", "ExampleTag", function(input, block) {
  paths <- str_trim(input@text)
  examples <- unlist(lapply(paths, readLines))

  tag(block, "examples") <- suffix(examples)
  block
})

setMethod("getPrereqs", "ExampleTag", function(tag) "ExampleTag")
