setClass("TagDev", contains = "Tag")

setMethod("procBlock", "TagDev", function(tag, block) {
  desc <- "This function is useful only for developers"
  
  modify_tags(block,
    title = c(text = prefix("[DEV] ")),
    description = c(text = prefix(desc)))
})

setMethod("getPrereqs", "TagDev", function(tag) {
  c("TagIntro", "TagTitle", "TagDetails")
})
