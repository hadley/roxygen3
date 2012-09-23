#' @@aliases: additional topic aliases.
#'
#' Add additional aliases through which the user can find the documentation
#' with \code{\link{help}}. The topic name is always included in the list of
#' aliases.
#'
#' @tagUsage @@aliases space separated aliases
setClass("AliasesTag", contains = "Tag")

setMethod("value<-", "AliasesTag", function(tag, value) {
  tag@text <- unlist(str_split(value, "[[:space:]]+"))
  tag
})

setMethod("writeRd", "AliasesTag", function(object) {
  RdCommand("alias", object@text)
})

setMethod("defaultTag", c("AliasesTag", "PackageObject"),
  function(tag, object) {
    if (str_detect(object@name, fixed("-package"))) return()
    new("AliasesTag", text = str_c(object@name, "-package"))
  }
)
