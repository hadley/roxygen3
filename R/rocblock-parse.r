# This makes it possible to create parsers that work with other components of
# the rocblocks, or they could ever ignore the input rocblocks altogether and
# look for data in other locations - only using the rocblocks as output.
parse_rocblocks <- function(parser, rocblocks) {
  UseMethod("parse_rocblocks")
}

# And you can always provide a list of parsers.
parse_rocblocks.list <- function(parser, rocblocks) {
  out <- lapply(parser, parse_rocblocks, rocblocks = rockblocks)
  recursive_merge(out)
}
