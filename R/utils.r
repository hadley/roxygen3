compact <- function(x) Filter(Negate(is.null), x)

"%||%" <- function(a, b) if (is.null(a)) b else a

recursive_merge <- function(lists) {
  lists <- compact(lists)
  if (length(lists) <= 1) return(lists)
  Reduce(modifyList, lists)
}


list_matrix <- function(lists) {
  stopifnot(is.list(lists))

  is_list <- vapply(lists, is.list, logical(1))
  stopifnot(all(is_list))
  
  n <- unique(vapply(lists, length, integer(1)))
  stopifnot(length(n) > 1)
  
  matrix(unlist(lists, recursive = FALSE), ncol = n, nrow = length(lists))
}
