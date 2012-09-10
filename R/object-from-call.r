#' Given a call that modifies the R environment, find the object that 
#' it creates.
#'
#' @details
#' \code{object_from_call} works in a pseudo-S3 manner - given a call like
#' \code{f(a, b, c)} it will call \code{object_from_call.f}.
#'
#' @param call unevaluated function call
#' @param env environment in which to evaluate function call
#' @return a list giving the \code{name} and \code{value} of the object
#'   that the call creates.  \code{NULL} is returned if the call doesn't
#'   modify the package environment in a way that roxygen recognises.
#' @keywords internal
#' @examples
#' a <- 1
#' object_from_call(quote(a <- 1), environment())
#' @autoImports
#' @export
#' @dev
object_from_call <- function(call, env, srcref) {
  if (is.null(call)) return(new("ObjectNull"))
  
  # Find function, then use match.call to construct complete call
  f <- eval(call[[1]], env)
  if (!is.primitive(f)) {
    call <- match.call(eval(call[[1]], env), call)
  }
  
  fun_name <- deparse(call[[1]])
  f <- find_fun(str_c("object_from_call.", fun_name))
  if (is.null(f)) return(new("ObjectNull"))
  
  out <- f(call, env)
  if (is.null(out)) return(new("ObjectNull"))

  new("RoxyObject", value = out$value, name = out$name, srcref = srcref)
}


object_from_call_assignment <- function(call, env) {
  name <- as.character(call[[2]])
  
  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(name) > 1)  return()
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(name, env)) return()
  
  val <- get(name, env)
  val <- add_s3_metadata(val, name, env)
  
  # Figure out if it's an s3 method or generic and add that info.
  if (is_s3_generic(name, env)) {
    class(val) <- "s3generic"
  } else if (is_s3_method(name, env)) {
    class(val) <- "s3method"
  }

  list(name = name, value = val)
}
"object_from_call.<<-" <- object_from_call_assignment
"object_from_call.<-" <- object_from_call_assignment
"object_from_call.=" <- object_from_call_assignment


#' @autoImports
object_from_call.setClass <- function(call, env) {
  name <- as.character(call$Class)
  val <- getClass(name, where = env)
  list(name = name, value = val)
}

#' @autoImports
object_from_call.setMethod <- function(call, env) {
  name <- as.character(call$f)
  val <- getMethod(name, eval(call$signature), where = env)
  list(name = name, value = val)
}

#' @autoImports
object_from_call.setRefClass <- function(call, env) {
  name <- as.character(call$Class)
  val <- getRefClass(name, where = env)
  list(name = name, value = val)
}

#' @autoImports
object_from_call.setGeneric <- function(call, env) {
  name <- as.character(call$name)
  val <- getGeneric(name, where = env)
  list(name = name, value = val)
}
