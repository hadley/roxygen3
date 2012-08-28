#' Create a roccer object.
#'
#' The roccer object is a key component in roxygen3 - it defines the behaviour
#' of a tag with a \code{parser} and a \code{output} write.
#' 
#' @dev
#' @param name string giving the tag name. Do not include "@" in the name.
#' @param parser parser to use to parse the tag. Currently roxygen3 only
#'   provides one parser: \code{\link{roc_parser}}.
#' @param output output generated, like an object created by
#'   \code{\link{rd_out}} or \code{\link{namespace_out}}.
#' @export
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

#' @export
is.roccer <- function(x) inherits(x, "roccer")

#' @export
print.roccer <- function(x, ...) {
  cat("Roccer: ", x$name, "\n", sep = "")
}

basic_roccer <- function(name, input, command = name) {
  roccer(name,
    roc_parser(tag = input),
    rd_out(rd_command(command))
  )
}

#' @export
find_roccers <- function(env = asNamespace("roxygen3")) {
  rocs <- compact(lapply(ls(env), function(x) {
    obj <- get(x, env)
    if (is.roccer(obj)) obj
  }))
  names(rocs) <- vapply(rocs, "[[", "name", FUN.VALUE = character(1))
  rocs
}

#' @auto_import
sort_roccers <- function(roccers, prereqs = NULL) {
  if (is.null(prereqs)) return(roccers)
  
  graph <- graph_from_list(names(roccers), prereqs)
  roccers[topo_sort(graph)]
}

base_roccers <- function() {
  base <- find_roccers(asNamespace("roxygen3"))
  sort_roccers(base, base_prereqs)
}

base_prereqs <- list()