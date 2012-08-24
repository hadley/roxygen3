
# L ← Empty list that will contain the sorted nodes
# S ← Set of all nodes with no outgoing edges
# for each node n in S do
#     visit(n) 
# function visit(node n)
#     if n has not been visited yet then
#         mark n as visited
#         for each node m with an edge from m to n do
#             visit(m)
#         add n to L
topo_sort <- function(roccers, prereqs = NULL) {
  
}

base_roccers <- function() {
  topo.sort(find_roccers(asNamespace("roxygen3")), base_prereqs)
}