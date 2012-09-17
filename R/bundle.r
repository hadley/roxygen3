setMethod("process", "Bundle", function(input) {
  # Process each block individually for local tags
  input@blocks <- lapply(input@blocks, cached_process)

  # Run global processors
  for (process in input@behaviour@processors) {
    input <- cached_processor(process, input)
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

cached_processor <- memoise(function(process, input) {
  f <- match.fun(process)
  f(input)
})
