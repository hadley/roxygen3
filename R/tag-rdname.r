setClass("TagRdname", contains = "Tag")
setMethod("defaultTag", c("TagRdname", "RoxyObject"),
  function(tag, object) {
    if (length(object@name) == 0) return()
    new("TagRdname", text = nice_name(object@name))
  }
)

subs <- matrix(ncol = 2, byrow = T, c(
  '[]', 'sub',
  '<-', 'set',
  '!', 'not',
  '"', 'quote',
  '#', 'hash',
  '$', 'cash',
  '%', 'grapes',
  '&', 'and',
  '|', 'or',
  "'", 'single-quote',
  '(', 'open-paren',
  ')', 'close-paren',
  '*', 'star',
  '+', 'plus',
  ',', 'comma',
  '/', 'slash',
  ':', 'colon',
  ';', 'semi-colon',
  '<', 'less-than',
  '=', 'equals',
  '>', 'greater-than',
  '?', 'p',
  '@', 'at',
  '[', 'open-brace',
  '\\', 'backslash',
  ']', 'close-brace',
  '^', 'hat',
  '`', 'tick',
  '{', 'open-curly',
  '}', 'close',
  '~', 'twiddle'
))
subs[, 2] <- str_c("-", subs[, 2])

nice_name <- function(x) {
  for(i in seq_len(nrow(subs))) {
    x <- str_replace_all(x, fixed(subs[i, 1]), subs[i, 2])
  }
  x <- str_replace(x, "-+", "-")
  
  # From renzao: https://github.com/klutometis/roxygen/pull/95
  # remove leading '-'
  x <- str_replace(x, "^-", "")
  # substitute leading '.'
  x <- str_replace(x, "^\\.", "dot-")
  x
}
