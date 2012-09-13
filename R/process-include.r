#' Control order of collation.
#'
#' @details
#' The collation order is only modified if it is different from alphabetical
#' order (in the C locale). That is R's default ordering.
#'
#' @usageTag @@include file-1.r file-2.r

# Need to be able to run this separately from the others, preferrably
# without parsing the code - because correct collate is needed before you
# can source the code.
process_include <- function(bundle, path = rPath(bundle)) {
  if (is.null(path)) return(bundle)

  old <- setwd(path)
  on.exit(setwd(old))

  # Build list of all R files
  r_paths <- dir(pattern = "\\.[RrSsQs]$")

  # Build named list of files to be included
  includes <- list()
  blocks <- bundle@blocks
  for(i in seq_along(blocks)) {
    tags <- blocks[[i]]@tags
    if (is.null(tags$include)) next

    path <- getSrcFilename(blocks[[i]]@srcref)
    include <- unlist(str_split(tags$include@text, " "))

    exists <- file.exists(include)
    if (any(!exists)) {
      message("Can't find @include file: ",
        str_c(include[!exists], collapse = ", "))
      include <- include[exists]
    }

    includes[[path]] <- c(includes[[path]], include)
  }

  with_collate("C", {
    r_paths <- sort(r_paths)
    collate <- collate_from_includes(includes, r_paths)
  })

  if (is.null(collate)) return(bundle)

  tag <- new("CollateTag", files = collate)
  block <- new("Block", tags = list(tag))

  bundle@blocks <- c(bundle@blocks, list(block))
  bundle
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

