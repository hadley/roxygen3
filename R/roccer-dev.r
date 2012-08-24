parse_dev <- function(roc, ...) {  
  if (is.null(roc$dev)) return(list())
  list(
    title = paste("[DEV]", roc$title),
    description = c("This function is useful only for developers",
      roc$description),
    dev = NULL)
}

roc_dev <- roccer("@dev", roc_parser(one = parse_dev))
base_prereqs[["@dev"]] <- c("@title", "@details")