roccer <- function(name, parser = NULL, output = NULL) {
  
  # Copy name into parser and output so they have access to it
  if (!is.null(parser) && is.null(parser$name)) {
    parser$name <- name
  }
  if (!is.null(output) && is.null(output$name)) {
    output$name <- name
  }

  structure(list(name = name, parser = parser, output = output), 
    class = "roccer")
}

is.roccer <- function(x) inherits(x, "roccer")

print.roccer <- function(x, ...) {
  cat("Roccer: ", x$name, "\n", sep = "")
}

basic_roccer <- function(name, input, command = NULL) {
  if (is.null(command)) {
    command <- str_sub(name, 2, -1)
  }
  
  roccer(name,
    roc_parser(tag = input),
    rd_out(rd_command(command))
  )
}

find_roccers <- function(env = asNamespace("roxygen3")) {
  rocs <- compact(lapply(ls(env), function(x) {
    obj <- get(x, env)
    if (is.roccer(obj)) obj
  }))
  names(rocs) <- vapply(rocs, "[[", "name", FUN.VALUE = character(1))
  rocs
}

