#' Add additional topic aliases.
#' 
#' Add additional aliases through which the user can find the documentation
#' with \code{\link{help}}. The topic name is always included in the list of
#' aliases.
#'
#' @tagUsage @@aliases space separated aliases

setClass("TagAliases", contains = "Tag")

setMethod("procTag", "TagAliases", function(tag) {
  parse_words(tag, min = 1)
})

setMethod("writeRd", "TagAliases", function(object) {
  new_command("alias", object@text)
})

setMethod("defaultTag", c("TagAliases", "PackageObject"), 
  function(tag, object) {
    new("TagAliases", text = str_c(object@name, "-package"))
  }
)
