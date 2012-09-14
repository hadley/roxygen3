split_pieces <- function(tag, text, split_with, min, max) {
  pieces <- unlist(str_split(text, split_with))
  pieces <- pieces[pieces != ""]

  if (length(pieces) < min) {
    stop(tag_name(tag), " requires at least ", min, " values.")
  }
  if (length(pieces) > max) {
    stop(tag_name(tag), " takes at most ", max, " values.")
  }

  pieces
}

parse_words <- function(tag, text, min = 0, max = Inf) {
  split_pieces(tag, text, "[[:space:]]+", min, max)
}

parse_name_desc <- function(tag) {
  pieces <- str_split_fixed(tag@text, "[[:space:]]+", 2)

  name <- pieces[, 1]
  desc <- str_trim(pieces[, 2])

  if (any(name == "")) {
    stop(tag_name(tag), ' requires a name and description')
  }
  setNames(desc, name)
}


