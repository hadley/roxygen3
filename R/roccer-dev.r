parse_dev <- function(roc, ...) {    
  list(
    title = paste("[DEV]", roc$title),
    description = c("This function is useful only for developers",
      roc$description))
}

roc_dev <- roccer("@dev", roc_parser(one = parse_dev))
base_prereqs[["@dev"]] <- c("@title", "@details")