setMethod("process", "Bundle", function(input) {
  # Process each block individually for local tags
  input@blocks <- lapply(input@blocks, process)

  # Run global processors
  for (process in input@behaviour@processors) {
    f <- match.fun(process)
    input <- f(input)
  }

  # Write output
  for (writer in input@behaviour@writers) {
    f <- match.fun(writer)
    f(input)
  }

  invisible(input)
})

setMethod("process", "Block", function(input) {
  for (tag in names(input@tags)) {
    input <- procBlock(input@tags[[tag]], block = input)
  }

  input
})

setGeneric("procBlock", function(tag, block) {
  standardGeneric("procBlock")
}, valueClass = "Block")
