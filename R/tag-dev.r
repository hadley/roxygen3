setClass("DevTag", contains = "Tag")

setMethod("procBlock", "DevTag", function(tag, block) {
  desc <- "This function is aimed primarily at developers extending this package. You shouldn't need to use it for everyday operation of the package."

  title <- block@tags$title
  if (!is.null(title)) {
    title@text <- str_c("[DEV] ", title@text)
  }

  tag(block, "title") <- title
  tag(block, "description") <- prefix(desc)
  block
})

setMethod("getPrereqs", "DevTag", function(tag) {
  c("IntroTag", "TitleTag", "DetailsTag")
})
