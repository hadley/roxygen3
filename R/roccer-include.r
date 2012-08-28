# Need to be able to run this separately from the others, preferrably
# without parsing the code - because correct collate is needed before you
# can source the code.

# 

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
  
  rocblocks[[i]]$roc <- modify_list(rocblocks[[i]]$roc, 
    list(collate = collate))
  rocblocks
}

collate_from_includes <- function(includes, paths) {
  if (length(includes) == 0) return(NULL)
  
  used <- unique(c(names(includes), unlist(includes)))
  graph <- graph_from_list(used, includes)
  collate <- union(topo_sort(graph), paths)  
}


add_roccer("include",
  rocblock_parser(include)
)
add_roccer("collate",
  null_parser(),
  description_out(field("Collate"))
)

