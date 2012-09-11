setMethod("process", "RoxyPackage", function(input) {
  # Process each block individually for local tags
  input@blocks <- lapply(input@blocks, process)
  
  # Run global processors
  for (process in input@behaviour@processors) {
    f <- match.fun(process)
    input <- process(input)
  }

  # Write output
  for (writer in input@behaviour@writers) {
    f <- match.fun(writer)
    f(input)
  }

  invisible(input)
})

setMethod("process", "RoxyBlock", function(input) {
  for (i in seq_along(input@tags)) {
    if (is.character(input@tags[[i]])) browser()
    input@tags[[i]] <- procTag(input@tags[[i]])
    input <- procBlock(input@tags[[i]], block = input)
  }
  input
})

setGeneric("procBlock", function(tag, block) {
  standardGeneric("procBlock")
}, valueClass = "RoxyBlock")

setGeneric("procTag", function(tag) {
  standardGeneric("procTag")
}, valueClass = "Tag")
