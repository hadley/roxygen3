setClass("FamilyTag", contains = "Tag")

setMethod("value<-", "FamilyTag", function(tag, value) {
  tag@text <- unique(str_trim(value))
  tag
})
