#' A bundle of files in a package.
#'
#' The package class captures all the information about the package:
#' its name, path, and all the \code{\link{Block}}s that it contains.
#'
#' @autoImports
#' @inheritParams DirectoryBundle
#' @param path path of package root directory
#' @export
#' @dev
PackageBundle <- function(path, behaviour = default_behaviour()) {
  pkg <- as.package(path)

  load_all(pkg)
  blocks <- in_dir(pkg$path, {
    parse_directory("R", ns_env(pkg), tags = behaviour@tags)
  })

  new("PackageBundle",
    name = pkg$package,
    path = pkg$path,
    blocks = blocks,
    behaviour = behaviour)
}

setMethod("rPath", "PackageBundle", function(bundle) {
  file.path(bundle@path, "R")
})
