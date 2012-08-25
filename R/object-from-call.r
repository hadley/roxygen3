#' Given a call that modifies the R environment, find the object that 
#' it creates.
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
object_from_call <- function(call, env) {
  if (is.null(call)) return()
  
  # Find function, then use match.call to construct complete call
  f <- eval(call[[1]], env)
  if (!is.primitive(f)) {
    call <- match.call(eval(call[[1]], env), call)
  }
  
  fun_name <- deparse(call[[1]])
  
  if (fun_name %in% c("<<-", "<-", "=")) {
    name <- as.character(call[[2]])

    # If it doesn't exist (any more), don't document it.
    if (!exists(name, env)) return()
    
    val <- get(name, env)
  } else if (fun_name == "setClass") {
    name <- as.character(call$Class)
    val <- getClass(name, where = env)    
  } else if (fun_name == "setRefClass") {
    name <- as.character(call$Class)
    val <- getRefClass(object$value, where = env)    
  } else if (fun_name == "setGeneric") {
    name <- as.character(call$name)
    val <- getGeneric(name, where = env)
  } else if (fun_name == "setMethod") {
    name <- as.character(call$f)
    val <- getMethod(name, eval(call$signature), where = env)
  } else {
    return(NULL)
  }
  list(name = name, value = val)
}

# Need some way for users to register that a function causes side effects
