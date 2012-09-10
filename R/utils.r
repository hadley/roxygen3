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

#' @autoImports
write_if_different <- function(path, contents) {
  if (!file.exists(dirname(path))) {
    dir.create(dirname(path), showWarnings = FALSE)
  }
  
  if (same_contents(path, contents)) return(FALSE)
  
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

same_contents <- function(path, contents) {
  if (!file.exists(path)) return(FALSE)
  
  contents <- str_c(str_c(contents, collapse = "\n"), "\n")
  
  text_hash <- digest(contents, serialize = FALSE)
  file_hash <- digest(file = path)
  
  identical(text_hash, file_hash)
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

ref_location <- function(srcref) {
  if (is.null(srcref)) return("")

  file <- getSrcFilename(srcref)
  str_c(" @", file, ":", srcref[1], ":", srcref[5])
}


str_truncate <- function(x, width = getOption("width")) {
  lines <- str_split(x, "\n")[[1]]
  
  n <- str_length(lines[1])
  if (n <= width && length(lines) == 1) return(lines[1])
  
  str_c(str_sub(lines[1], 1, width - 3), "...")
}