setClass("TagExample", contains = "Tag")
setMethod("procBlock", "TagExample", function(tag, block) {
  paths <- str_trim(tag@text)
  examples <- unlist(lapply(paths, readLines))
  
  modify_tags(block, 
    examples = suffix(examples))
})

setMethod("getPrereqs", "TagExample", function(tag) "TagExamples")