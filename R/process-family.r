process_family <- function(package) {
  blocks <- package@blocks

  families <- lapply(blocks, tag_value, "family")
  has_family <- !vapply(families, is.null, logical(1))

  blocks <- blocks[has_family]
  families <- families[has_family]
  names(blocks) <- vapply(blocks, tag_value, "name", FUN.VALUE = character(1))
  names(families) <- names(blocks)

  family_lookup <- invert(families)

  for(family in names(family_lookup)) {
    related <- family_lookup[[family]]

    for(topic_name in related) {
      topic <- blocks[[topic_name]]
      others <- sort_c(setdiff(related, topic_name))

      if (length(others) < 1) next

      links <- str_c("\\code{\\link{", others, "}}", collapse = ", ")

      tag(topic, "seealso") <- suffix(str_c("Other ", family, ": ", links))
      blocks[[topic_name]] <- topic
    }
  }

  package@blocks[has_family] <- blocks
  package
}
