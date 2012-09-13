#' Block class.
#'
#' The block class encapsulates the data about a single roxygen comment
#' block, along with its location in the src code, and the object associated
#' with the block.
#'
#' @export
Block <- function(tags = list(), object = new("NullObject"), srcref = new("NullSrcref")) {

  # Automatically add default tags based on the object.
  super <- names(getClass(object@class)@contains)
  # Find possible methods
  tag_names <- unique(findMethodSignatures("defaultTag",
    classes = c(object@class, super))[, "tag"])
  methods <- lapply(tag_names, function(x) {
    selectMethod("defaultTag", c(x, object@class))
  })

  defaults <- compact(lapply(methods, call_fun, object = object))
  names(defaults) <- vapply(defaults, tag_name, character(1))
  tags <- c(tags, defaults[setdiff(names(defaults), names(tags))])

  new("Block", tags = tags, object = object, srcref = srcref)
}


setMethod("show", "Block", function(object) {
  cat("Block: ", object@object@name, "@",
    location(object@srcref), "\n", sep = "")
  lapply(object@tags, show)
})

setMethod("process", "Block", function(input) {
  for (tag in names(input@tags)) {
    input <- process(input@tags[[tag]], block = input)
  }

  input
})
