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
  if (is.null(call)) return(new("NullObject", srcref = srcref))

  # Find function, then use match.call to construct complete call
  f <- eval(call[[1]], env)
  if (!is.primitive(f)) {
    call <- match.call(eval(call[[1]], env), call)
  }

  class <- paste("Call", first_upper(deparse(call[[1]])), sep = "")

  method <- selectMethod("objectFromCall", c(call = class))
  method(call, env, srcref)
}

setGeneric("objectFromCall", function(call, env, srcref) {
  standardGeneric("objectFromCall")
})
setMethod("objectFromCall", "ANY", function(call, env, srcref) {
  new("NullObject", srcref = srcref)
})

object_from_assignment <- function(call, env, srcref) {
  name <- as.character(call[[2]])

  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(name) > 1)  return(new("NullObject"))

  # If it doesn't exist (any more), don't document it.
  if (!exists(name, env)) return(new("NullObject"))

  val <- get(name, env)
  val <- add_s3_metadata(val, name, env)

  # Figure out if it's an s3 method or generic and add that info.
  if (is_s3_generic(name, env)) {
    objtype <- "S3GenericObject"
  } else if (is_s3_method(name, env)) {
    objtype <- "S3MethodObject"
  } else if (is.function(val)) {
    objtype <- "FunctionObject"
  } else {
    objtype <- "DataObject"
  }

  new(objtype, name = name, value = val, srcref = srcref)
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

  new("S4ClassObject", name = name, value = val, srcref = srcref)
})

setClass("CallSetGeneric")
#' @autoImports
setMethod("objectFromCall", "CallSetGeneric", function(call, env, srcref) {
  name <- as.character(call$name)
  val <- getGeneric(name, where = env)

  new("S4GenericObject", name = name, value = val, srcref = srcref)
})

setClass("CallSetMethod")
#' @autoImports
setMethod("objectFromCall", "CallSetMethod", function(call, env, srcref) {
  name <- as.character(call$f)
  val <- getMethod(name, eval(call$signature), where = env)

  new("S4MethodObject", name = name, value = val, srcref = srcref)
})

setClass("CallSetRefClass")
#' @autoImports
setMethod("objectFromCall", "CallSetRefClass", function(call, env, srcref) {
  name <- as.character(call$Class)
  val <- getRefClass(name, where = env)

  new("R5ClassObject", name = name, value = val, srcref = srcref)
})
