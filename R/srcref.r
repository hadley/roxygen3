
location <- function(x) {
  if (isNull(x)) return("Unknown")

  path <- getSrcFilename(x)
  str_c(path, ":", x[1], ":", x[3], ":")
}
