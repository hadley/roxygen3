rocblock <- function(obj, roc, path, lines) {  
  structure(
    list(obj = obj, roc = roc, path = path, lines = lines),
    class = "rocblock"
  )
}

print.roc <- function(x, ...) {
  cat("rocblock: ", x$obj$name, " @ ",
    x$path, ":", x$lines[1], ":", x$lines[2], "\n", sep = "")
  str(x$roc, nchar.max = 40)
}
