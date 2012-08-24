compact <- function(x) Filter(Negate(is.null), x)

"%||%" <- function(a, b) if (is.null(a)) b else a

recursive_merge <- function(lists) {
  lists <- compact(lists)
  if (length(lists) <= 1) return(lists)
  Reduce(modifyList, lists)
}

write_if_different <- function(path, contents) {
  if (!file.exists(dirname(path))) {
    dir.create(dirname(path), showWarnings = FALSE)
  }
  
  contents <- str_c(contents, collapse = "\n")
  if (the_same(path, contents)) return(FALSE)
  
  name <- basename(path)
  if (!str_detect(name, "^[a-zA-Z][a-zA-Z0-9_.-]*$")) {
    cat("Skipping invalid path: ", name, "\n")
    FALSE
  } else {
    cat(sprintf('Writing %s\n', name))
    writeLines(contents, path)
    TRUE
  }  
}
the_same <- function(path, new) {
  if (!file.exists(path)) return(FALSE)

  old <- str_c(readLines(path), collapse = "\n")
  return(identical(old, new))
}

