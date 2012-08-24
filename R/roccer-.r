roccer <- function(name, parser = NULL, output = NULL) {
  
  # Copy name into parser and output so they have access to it
  if (is.null(parser$name)) {
    parser$name <- name
  }
  if (is.null(output$name)) {
    output$name <- name
  }

  structure(list(name = name, parser = parser, output = output), 
    class = "roccer")
}

is.roccer <- function(x) inherits(x, "roccer")

basic_roccer <- function(name, input, command = NULL) {
  if (is.null(command)) {
    command <- substr(name, 2, -1)
  }
  
  roccer(name, 
    roc_parser(tag = input),
    rd_out(rd_command(command))
  )
}

