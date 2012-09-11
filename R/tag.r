#' Tag class
#' 
#' The tag class is the base class for all roxygen3 tags. 
#'
#' @section Parsing order:
#' The functions \code{procTag} and \code{procBlock} are called in that
#' order, so if both are supplied, \code{procBlock} can rely on the 
#' tag already being parsed.
setMethod("show", "Tag", function(object) {
  tag <- tag_name(object)
  
  out <- str_c("@", tag, " ", str_c(object@text, collapse = "\n"))
  cat(str_truncate(out), "\n", sep = "")
})

# Default behaviour for all tags: don't change and no prereqs.
setMethod("procBlock", "Tag", function(tag, block) block)
setMethod("procTag", "Tag", function(tag) tag)

tag_name <- function(x) {
  if (isS4(x)) {
    class <- getClass(class(x))@className
  } else if (is.character(x)) {
    class <- x
  }
  first_lower(str_replace(class, "^Tag", ""))
}

#' @export
find_tags <- function() {
  names(getClass("Tag")@subclasses)
}

#' @autoImports
sort_tags <- function(tags, prereqs = NULL) {
  if (is.null(prereqs)) return(tags)
  
  graph <- graph_from_list(tags, prereqs)
  topo_sort(graph)
}

base_tags <- function() {
  base <- find_tags()
  methods <- findMethods("getPrereqs", classes = base)
  prereqs <- lapply(methods, call_fun)
  
  tag_name(sort_tags(base, prereqs))
}
