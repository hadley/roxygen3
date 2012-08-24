split_pieces <- function(text, split_with, min, max) {
  pieces <- str_split(text, split_with)
  
  if (length(pieces) < min) {
    stop(key, " requires at least ", min, " values.")
  } 
  if (length(pieces) > max) {
    stop(key, " takes at most ", max, " values.")
  }

  pieces
}

words_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, "[[:space:]]+", min, max)
  }
}

# all basically work like words_tag, except they split on different things.
arguments_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, ", ?", min, max)
  }
}

sentence_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, "[.?!]", min, max)
  }
}

paragraph_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, "\n{2,}", min, max)
  }
}

name_desc_tag <- function(text, key, srcref) {
  pieces <- str_split_fixed(text, "[[:space:]]+", 2)
  
  name <- pieces[, 1]
  desc <- str_trim(pieces[, 2])

  if (name == "") {
    roxygen_stop(key, ' requires a name and description', srcref = srcref)
  }
  
  list(name = name, desc = desc)
}


