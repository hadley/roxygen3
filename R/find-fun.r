find_fun <- function(name, env = parent.frame()) {
  pkg <- asNamespace("roxygen3")
  if (exists(name, pkg)) {
    return(get(name, pkg))
  }

  if (exists(name, env)) {
    return(get(name, env))
  }

  
  NULL
}
