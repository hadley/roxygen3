#' @auto_imports
section_tag <- function(text, key) {
  pieces <- str_split_fixed(text, ":", n = 2)[1, ]
  list(list(name = pieces[1], content = pieces[2]))
}

roc_section <- basic_roccer("section", section_tag)

