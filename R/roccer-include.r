# Need to be able to run this separately from the others, preferrably
# without parsing the code - because correct collate is needed before you
# can source the code.

include <- function(rocblocks) {
  with_collate("C", {
    dirs <- unique(vapply(rocblocks, function(x) dirname(x$path),
      character(1)))
    paths <- dir(dirs, pattern = "\\.[RrSsQs]$", full.names = TRUE)

    includes <- list()
    for(i in seq_along(rocblocks)) {
      roc <- rocblocks[[i]]$roc
      path <- rocblocks[[i]]$path
    
      if (is.null(roc$include)) next
    
      include <- file.path(dirname(path), str_split(roc$include, " ")[[1]])
      
      exists <- file.exists(include)
      if (any(!exists)) {
        message("Can't find @include file: ", 
          str_c(include[!exists], collapse = ", "))
        include <- include[exists]
      }
      
      includes[[path]] <- c(includes[[path]], include)
    }
  
    collate <- collate_from_includes(includes, paths)
  })
  collate <- str_replace(collate, "R/", "")
  
  rocblocks[[1]]$roc <- modify_list(rocblocks[[i]]$roc, 
    list(collate = collate))
  rocblocks
}

collate_from_includes <- function(includes, paths) {
  if (length(includes) == 0) return(NULL)
  if (is_topo_sorted(paths, includes)) return(NULL)
  
  used <- unique(c(names(includes), unlist(includes)))
  graph <- graph_from_list(used, includes)
  union(topo_sort(graph), paths)
}

is_topo_sorted <- function(nodes, edges) {
  # Check to see if topology already satified
  
  before <- function(a, b) {
    all(which(nodes %in% a) < which(nodes %in% b))
  }

  for (i in seq_along(edges)) {
    src <- edges[[i]]
    trg <- names(edges)[i]
    
    if (!before(src, trg)) return(FALSE)
  }
  
  TRUE
}

#' Control order of collation.
#'
#' @details 
#' The collation order is only modified if it is different from alphabetical
#' order (in the C locale). That is R's default ordering.
#' 
#' @@usage @@include file-1.r file-2.r
add_roccer("include",
  rocblock_parser(include)
)
add_roccer("collate",
  null_parser(),
  description_out(field("Collate"))
)

