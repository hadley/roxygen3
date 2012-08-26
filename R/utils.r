compact <- function(x) Filter(Negate(is.null), x)

"%||%" <- function(a, b) if (is.null(a)) b else a

modify_list <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  modifyList(a, b)
}

recursive_merge <- function(lists) {
  lists <- compact(lists)
  if (length(lists) <= 1) return(lists)
  Reduce(modify_list, lists)
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

invert <- function(x) {
  if (length(x) == 0) return()
  unstack(rev(stack(x)))
}

is.syntactic <- function(x) make.names(x) == x
has.quotes <- function(x) str_detect(x, "'|\"")
quote_if_needed <- function(x) {
  needs_quotes <- !has.quotes(x) & !is.syntactic(x)
  x[needs_quotes] <- str_c('"', x[needs_quotes], '"')
  x
}


find_function <- function(name, env = parent.frame(2)) {
  env <- parent.frame(2)
  if (!exists(name, envir = env, mode = "function")) {
    return(NULL)
  }
  
  get(name, envir = env, mode = "function")
}