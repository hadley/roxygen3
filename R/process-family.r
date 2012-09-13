roccer_family <- function(package) {
  return(package)

  # out <- rep(list(list()), length(rocblocks))
  # return(out)
  # names(rocblocks) <- vapply(rocblocks, function(x) x$name, character(1))
  #
  # families <- lapply(rocblocks, function(x) x$roc$family)
  #
  # family_lookup <- invert(families)
  # for(family in names(family_lookup)) {
  #   related <- family_lookup[[family]]
  #
  #   for(topic_name in related) {
  #     topic <- rocblocks[[topic_name]]
  #     others <- setdiff(related, topic_name)
  #
  #     if (length(others) < 1) next;
  #
  #     other_topics <- sort(unlist(name_lookup[others], use.names = FALSE))
  #     links <- paste("\\code{\\link{", other_topics, "}}",
  #       collapse =", ", sep = "")
  #     family <- paste("Other ", family, ": ", links, sep = "")
  #
  #     rocblocks[[topic_name]]$roc$seealso <- c(topic$seealso, family)
  #   }
  # }
  #
  # rocblocks
}
