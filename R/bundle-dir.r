setClass("DirectoryBundle", contains = "Bundle", representation(
  path = "character")
)

DirectoryBundle <- function(path, behaviour = default_behaviour()) {
  blocks <- in_dir(path, parse_directory("."))

  new("DirectoryBundle",
    path = path,
    blocks = blocks,
    behaviour = behaviour)
}

