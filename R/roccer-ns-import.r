#' @auto_import
ns_import_from <- roccer("importFrom", 
  roc_parser(
    function(tag, name) {
      # @TODO: reinstate once prereq sorting in place.
      # pieces <- str_split(tag, "[[:space:]]+")[[1]]
      # if (length(pieces) < 2) {
      #   stop("@importFrom needs at least two components.", call. = FALSE)
      # }
      # setNames(pieces[-1], pieces[1])
      return(tag)
    }
  ),
  namespace_out(function(tag) {
    str_c("importFrom(", quote_if_needed(tag), ",", names(tag), ")")
  })
)

#' @auto_import
ns_auto_import <- roccer("auto_import", 
  roc_parser(one = 
    function(roc, obj, ...) {
      if (is.null(roc$auto_import)) return(list())
      if (!is.function(obj$value)) return(list())
      
      auto <- auto_imports(obj$value, obj$name, roc$importFrom)
      list(importFrom = c(auto, roc$importFrom), auto_import = NULL)
    }
  )
)

base_prereqs[["auto_import"]] <- "importFrom"

#' Scans a function and determines what functions it uses from other packages.
#' 
#' @param f function to process
#' @param imported list of functions that are already imported
#' @auto_import
auto_imports <- function(f, name, imported = NULL, loaded = NULL) {
  globals <- findGlobals(f, merge = FALSE)$functions
  this_pkg <- environmentName(environment(f))

  if (is.null(loaded)) {    
    loaded <- loaded_funs(this_pkg)
  }
  matches <- setdiff(intersect(names(loaded), globals), imported)
  source <- loaded[matches]

  source_length <- vapply(source, length, integer(1))
  ambiguous <- source_length > 1
  if (any(ambiguous)) {
    pkgs <- lapply(source[ambiguous], paste, collapse = ", ")

    message("In ", name, ", multiple possible sources for: \n", 
      paste("  ", names(pkgs), ": ", pkgs, collapse = "\n", sep = ""))
  }

  vapply(source, "[", 1, FUN.VALUE = character(1))
}

loaded_funs <- function(exclude = NULL) {
  base <- c("base", exclude)
  pkgs <- setdiff(loadedNamespaces(), base)
  envs <- setNames(lapply(pkgs, asNamespace), pkgs)

  invert(lapply(envs, ls))
}
