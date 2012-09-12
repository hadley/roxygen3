setClass("TagDev", contains = "Tag")

setMethod("procBlock", "TagDev", function(tag, block) {
  desc <- "This function is aimed primarily at developers extending this package. You shouldn't need to use it for everyday operation of the package."

  title <- block@tags$title
  if (!is.null(title)) {
    title@text <- str_c("[DEV] ", title@text)
  }

  modify_tags(block,
    title = title,
    description = prefix(desc))
})

setMethod("getPrereqs", "TagDev", function(tag) {
  c("TagIntro", "TagTitle", "TagDetails")
})
