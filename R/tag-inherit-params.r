#' @@inheritParams: Inherit parameters from another function.
#'
#' This tag will bring in all documentation for parameters that are
#' undocumented in the current function, but documented in the source
#' function. The source can be a function in the current package,
#' \code{function}, or another package \code{package::function}.
#'
#' You may use multiple \code{@@inheritParams} tags to inherit parameters
#' from multiple functions.
#'
#' @tagUsage
#'   @@inheritParams local_function
#'   @@inheritParams package::remote_function
setClass("InheritParamsTag", contains = "Tag")

setMethod("defaultTag", c("InheritParamsTag", "S4MethodObject"),
  function(tag, object) {
    method <- object@value

    pkg <- getPackageName(environment(object@value))
    gen_pkg <- attr(method@generic, "package")

    inherit <- as.character(method@generic)
    if (pkg != gen_pkg) {
      inherit <- str_c(gen_pkg, "::", inherit)
    }
    new("InheritParamsTag", text = inherit)
  }
)

setMethod("defaultTag", c("InheritParamsTag", "S3MethodObject"),
  function(tag, object) {
    method <- object@value

    gen_name <- attr(method, "s3method")[1]
    gen <- get(gen_name, attr(method, "s3env"))

    pkg <- getPackageName(environment(object@value))
    if (is.primitive(gen)) {
      gen_pkg <- "base"
    } else {
      gen_pkg <- getPackageName(environment(gen))
    }

    inherit <- gen_name
    if (pkg != gen_pkg) {
      inherit <- str_c(gen_pkg, "::", inherit)
    }
    new("InheritParamsTag", text = inherit)
  }
)
