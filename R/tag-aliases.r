#' Add additional topic aliases.
#'
#' Add additional aliases through which the user can find the documentation
#' with \code{\link{help}}. The topic name is always included in the list of
#' aliases.
#'
#' @usageTag @@aliases space separated aliases

setClass("AliasesTag", contains = "Tag")

setMethod("value<-", "AliasesTag", function(tag, value) {
  tag@text <- parse_words(tag, value, min = 1)
  tag
})

setMethod("writeRd", "AliasesTag", function(object) {
  new_command("alias", object@text)
})

setMethod("defaultTag", c("AliasesTag", "PackageObject"),
  function(tag, object) {
    new("AliasesTag", text = str_c(object@name, "-package"))
  }
)
