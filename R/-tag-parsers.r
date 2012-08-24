split_pieces <- function(text, key, split_with, min, max) {
  pieces <- str_split(text, split_with)[[1]]
  
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
    split_pieces(text, key, "[[:space:]]+", min, max)
  }
}

# all basically work like words_tag, except they split on different things.
arguments_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, key, ", ?", min, max)
  }
}

sentence_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, key, "[.?!]", min, max)
  }
}

paragraph_tag <- function(min = 0, max = Inf) {
  function(text, key, ...) {
    split_pieces(text, key, "\n{2,}", min, max)
  }
}

text_tag <- function() {
  function(text, key, ...) text
}

name_desc_tag <- function() {
  function(text, key, srcref) {
    pieces <- str_split_fixed(text, "[[:space:]]+", 2)
  
    name <- pieces[, 1]
    desc <- str_trim(pieces[, 2])

    if (any(name == "")) {
      stop(key, ' requires a name and description')
    }
    setNames(desc, name)
  }
}


