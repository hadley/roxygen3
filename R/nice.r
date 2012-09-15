nice_name <- function(x) nice(x, name_subs)
nice_alias <- function(x) nice(x, alias_subs)

nice <- function(x, subs) {
  for(i in seq_along(subs)) {
    x <- str_replace_all(x, fixed(names(subs)[i]), subs[i])
  }
  x <- str_replace(x, "-+", "-")

  # From renzao: https://github.com/klutometis/roxygen/pull/95
  # remove leading '-'
  x <- str_replace(x, "^-", "")
  # substitute leading '.'
  x <- str_replace(x, "^\\.", "dot-")
  x
}

name_subs <- c(
  '[]' = 'sub',
  '<-' = 'set',
  '!' = 'not',
  '"' = 'quote',
  '#' = 'hash',
  '$' = 'cash',
  '%' = 'grapes',
  '&' = 'and',
  '|' = 'or',
  "'" = 'single-quote',
  '(' = 'open-paren',
  ')' = 'close-paren',
  '*' = 'star',
  '+' = 'plus',
  ',' = '',
  '/' = 'slash',
  ':' = 'colon',
  ';' = 'semi-colon',
  '<' = 'less-than',
  '=' = 'equals',
  '>' = 'greater-than',
  '?' = 'p',
  '@' = 'at',
  '[' = 'open-brace',
  '\\' = 'backslash',
  ']' = 'close-brace',
  '^' = 'hat',
  '`' = 'tick',
  '{' = 'open-curly',
  '}' = 'close',
  '~' = 'twiddle',
  ' ' = ''
)
name_subs[] <- str_c("-", name_subs)

alias_subs <- name_subs[c("@", " ", "!", "|", "/")]
