#' Tag class
#'
#' The tag class is the base class for all roxygen3 tags.  To implement a new
#' tag, you will need to subclass this class.
#'
#' @dev
Tag <- function() {
  new("Tag")
}

setMethod("show", "Tag", function(object) {
  tag <- tag_name(object)

  out <- str_c("@", tag, " ", str_c(object@text, collapse = "\n"))
  cat(str_truncate(out), "\n", sep = "")
})

setMethod("format", "Tag", function(x, ...) format(value(x)))

# Default behaviour for all tags: don't change and no prereqs.
setMethod("process", "Tag", function(input, block) block)

setMethod("value<-", "Tag", function(tag, value) {
  tag@text <- value
  tag
})
setMethod("value", "Tag", function(tag) {
  tag@text
})
setMethod("isEmpty", "Tag", function(tag) {
  val <- value(tag)
  length(val) == 0 || identical(val, "")
})

find_tag <- memoise(function(name, tags = base_tags()) {
  tag <- tags[match(tolower(name), tolower(tags), nomatch = 0L)]
  if (length(tag) == 0) return()

  new(tag_class(tag))
})

tag_class <- function(name) {
  str_c(first_upper(name), "Tag")
}

tag_name <- function(x) {
  if (isS4(x)) {
    class <- getClass(class(x))@className
  } else if (is.character(x)) {
    class <- x
  }
  first_lower(str_replace(class, "Tag$", ""))
}

#' Find all currently defined tags.
#'
#' @keywords internal
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
