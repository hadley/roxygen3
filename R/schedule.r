#' @import igraph
roccer_graph <- function(roccers, prereqs) {
  nodes <- names(roccers)

  n <- vapply(prereqs, length, integer(1))
  edge_df <- cbind(unlist(prereqs), rep(names(prereqs), n))
  node_df <- data.frame(name = nodes)

  graph <- graph.data.frame(edge_df, vertices = node_df)
  V(graph)$label <- V(graph)$name

  graph
}

sort_roccers <- function(roccers, prereqs = NULL) {
  if (is.null(prereqs)) return(roccers)
  
  g <- roccer_graph(roccers, base_prereqs)
  order <- V(g)$name[topological.sort(g)]
  roccers[order]
}

base_roccers <- function() {
  base <- find_roccers(asNamespace("roxygen3"))
  sort_roccers(base, base_prereqs)
}