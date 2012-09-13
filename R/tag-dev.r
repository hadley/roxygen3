setClass("DevTag", contains = "Tag")

setMethod("process", "DevTag", function(input, block) {
  desc <- "This function is aimed primarily at developers extending this package. You shouldn't need to use it for everyday operation of the package."

  title@text <- str_c("[DEV] ", tag_value(block, "title"))
  tag(block, "title") <- title

  tag(block, "description") <- prefix(desc)
  block
})

setMethod("getPrereqs", "DevTag", function(tag) {
  c("IntroTag", "TitleTag", "DetailsTag")
})
