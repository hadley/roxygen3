compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

sort_c <- function(x) with_collate("C", sort(x))

"%||%" <- function(a, b) if (length(a) == 0) b else a

modify_list <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  modifyList(a, b)
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

str_truncate <- function(x, width = getOption("width")) {
  lines <- str_split(x, "\n")[[1]]

  n <- str_length(lines[1])
  if (n <= width && length(lines) == 1) return(lines[1])

  str_c(str_sub(lines[1], 1, width - 3), "...")
}

call_fun <- function(f, ...) f(...)

local_apropos <- function(x) {
  grep(x, ls(topenv(environment())), ignore.case = FALSE, value = TRUE)
}
first_upper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
first_lower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}
