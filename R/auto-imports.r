#' Scans a function and determines what functions it uses from other packages.
#' 
#' @details
#' If a function is available in two or more packages, then it is flagged
#' as a conflict, and it must be resolved by hand using \code{@@importFrom}.
#' The only exception is if a function of that name exists in the same
#' environment as f: in that case, no import statement will be generated.
#' 
#' All packages (including recommend packages like stats and graphics) need
#' explicit imports in the \file{NAMESPACE} file. The only exception is the 
#' base package.
#'
#' @param f function to process
#' @param name name of function. If omitted, defaults to the deparsed
#'   call to \code{f} - this is only useful for interactive use.
#' @param imported list of functions that are already imported
#' @param imports list of imported packages. If omitted, \code{auto_imports}
#'   will attempted to determine automatically by inspecting the the 
#'   \file{DESCRIPTION} of the package that \code{f} belongs to.
#' @autoImports
#' @dev
auto_imports <- function(f, name = deparse(substitute(f)), imported = NULL, imports = NULL) {
  globals <- findGlobals(f, merge = FALSE)$functions
  this_pkg <- environment(f)
  
  if (is.null(imports)) {
    pkg_path <- getNamespaceInfo(this_pkg, "path")
    imports <- package_imports(pkg_path)
  }

  # Remove functions defined in the current package, or have already
  # been imported.
  globals <- setdiff(globals, ls(this_pkg))
  globals <- setdiff(globals, imported)
  
  # Where do these functions come from?
  loaded <- loaded_funs(imports)
  # No known location
  unknown <- setdiff(globals, names(loaded))
  if (length(unknown) > 0) {
    message("Can not find functions: ", str_c(unknown, collapse = ","),
      ". Are you missing an Imports in your description?")
  }
  
  # Multiple locations
  sources <- loaded[intersect(names(loaded), globals)]
  locs <- vapply(sources, length, integer(1))
  conflicts <- locs > 1
  if (any(conflicts)) {
    pkgs <- lapply(sources[conflicts], paste, collapse = ", ")
    message("In ", name, ", multiple possible sources for: \n", 
      str_c("  ", names(pkgs), ": ", unlist(pkgs), collapse = "\n"))
  }
  
  # Remove base functions - they don't need to be imported
  only_base <- function(x) identical(x, "base")
  non_base <- Filter(Negate(only_base), sources)
  vapply(non_base, "[", 1, FUN.VALUE = character(1))
}

loaded_funs <- memoise(function(pkgs) {
  envs <- setNames(lapply(pkgs, asNamespace), pkgs)
  exports <- lapply(envs, function(x) ls(getNamespaceInfo(x, "exports")))
  exports$base <- ls("package:base")
  invert(exports)
})
package_imports <- memoise(function(path) {
  info <- as.list(read.dcf(file.path(path, "DESCRIPTION"))[1, ])
  devtools:::parse_deps(info$Imports)$name
})