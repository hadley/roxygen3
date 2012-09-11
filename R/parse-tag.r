split_pieces <- function(tag, split_with, min, max) {
  pieces <- unlist(str_split(tag@text, split_with))
  
  if (length(pieces) < min) {
    stop(tag_name(tag), " requires at least ", min, " values.")
  } 
  if (length(pieces) > max) {
    stop(tag_name(tag), " takes at most ", max, " values.")
  }
  
  tag@text <- pieces
  tag
}

parse_words <- function(tag, min = 0, max = Inf) {
  split_pieces(tag, "[[:space:]]+", min, max)
}

parse_arguments <- function(tag, min = 0, max = Inf) {
  split_pieces(tag, ",[[:space:]]*", min, max)
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


