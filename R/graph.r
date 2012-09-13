#' @autoImports
graph_from_list <- function(nodes, edges) {
  n <- vapply(edges, length, integer(1))
  edge_df <- cbind(unlist(edges), rep(names(edges), n))
  node_df <- data.frame(name = nodes)

  missing_nodes <- edge_df[!(edge_df %in% nodes)]
  if (length(missing_nodes) > 1) {
    stop("Edges refer to missing nodes ",
      str_c(missing_nodes, collapse = ", "), call. = FALSE)
  }

  graph <- graph.data.frame(edge_df, vertices = node_df)
  V(graph)$label <- V(graph)$name

  graph
}

#' @autoImports
topo_sort <- function(graph) {
  V(graph)$name[topological.sort(graph)]
}
