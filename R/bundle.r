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

setMethod("rPath", "Bundle", function(bundle) {
  NULL
})

cached_process <- memoise(function(process, input) {
  f <- match.fun(process)
  f(input)
})
