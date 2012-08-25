rocblock <- function(obj, roc, path, lines) {  
  structure(
    list(obj = obj, roc = roc, path = path, lines = lines),
    class = "rocblock"
  )
}

print.rocblock <- function(x, ...) {
  path <- if (is.null(x$path)) "[text]" else x$path
  cat("rocblock: ", x$obj$name, "@",
    path, ":", x$lines[1], ":", x$lines[2], "\n", sep = "")
  str(x$roc, nchar.max = 40)
}
