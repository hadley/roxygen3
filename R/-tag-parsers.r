words_tag <- function(min = 0, max = Inf) {
  function(text, key, srcref) {
    pieces <- str_split(text, "[[:space:]]+")
    
    if (length(pieces) < min) {
      roxygen_stop(key, " requires at least ", min, " values.", 
        srcref = srcref)
    } 
    if (length(pieces) > max) {
      roxygen_stop(key, " takes at most ", max, " values.", srcref = srcref)
    }

    pieces
  }
  
}

# all basically work like words_tag, except they split on different things.
arguments_tag <- function() {
  
}
sentence_tag <- function() {
  
}
paragraph_tag <- function() {
  
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


