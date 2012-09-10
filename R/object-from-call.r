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
  
  class <- paste("Call", first_upper(deparse(call[[1]])))
  
  method <- selectMethod("objectFromCall", c(call = class))
  method(call, env, srcref)
}

setGeneric("objectFromCall", function(call, env, srcref) {
  standardGeneric("objectFromCall")
})
setMethod("objectFromCall", "ANY", function(call, env, srcref) {
  new("ObjectNull")
})

object_from_assignment <- function(call, env, srcref) {
  name <- as.character(call[[2]])
  
  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(name) > 1)  return(new("ObjectNull"))
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(name, env)) return(new("ObjectNull"))
  
  val <- get(name, env)
  val <- add_s3_metadata(val, name, env)
  
  # Figure out if it's an s3 method or generic and add that info.
  if (is_s3_generic(name, env)) {
    doctype <- "s3generic"
  } else if (is_s3_method(name, env)) {
    doctype <- "s3method"
  } else {
    doctype <- "function"
  }

  new("RoxyObject", name = name, value = val, srcref = srcref, 
    docType = doctype)
}
setClass("Call<<-")
setMethod("objectFromCall", "Call<<-", object_from_assignment)
setClass("Call<-")
setMethod("objectFromCall", "Call<-", object_from_assignment)
setClass("Call=")
setMethod("objectFromCall", "Call=", object_from_assignment)

setClass("CallSetClass")
#' @autoImports
setMethod("objectFromCall", "CallSetClass", function(call, env, srcref) {
  name <- as.character(call$Class)
  val <- getClass(name, where = env)
  
  new("RoxyObject", name = name, value = value, srcref = srcref, 
    docType = "s4class")
})

setClass("CallSetGeneric")
#' @autoImports
setMethod("objectFromCall", "CallSetGeneric", function(call, env, srcref) {
  name <- as.character(call$name)
  val <- getGeneric(name, where = env)

  new("RoxyObject", name = name, value = value, srcref = srcref, 
    docType = "s4generic")
})

setClass("CallSetMethod")
#' @autoImports
setMethod("objectFromCall", "CallSetMethod", function(call, env, srcref) {
  name <- as.character(call$f)
  val <- getMethod(name, eval(call$signature), where = env)

  new("RoxyObject", name = name, value = value, srcref = srcref, 
    docType = "s4method")
})

setClass("CallSetRefClass")
#' @autoImports
setMethod("objectFromCall", "CallSetRefClass", function(call, env, srcref) {
  name <- as.character(call$Class)
  val <- getRefClass(name, where = env)

  new("RoxyObject", name = name, value = value, srcref = srcref, 
    docType = "r5class")
})