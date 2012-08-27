usage.s3generic <- usage.function

usage.s3method <- function(obj, name) {
  args <- usage_args(formals(obj))
  
  new_usage(
    method = s3_method_info(obj),
    args = args, 
    subclass = "usage_s3method")
}

format.usage_s3method <- function(x) {
  arglist <- args_string(x$args)

  method <- function(x) {
    str_c("\\method", "{", x[1], "}{", x[2], "}", collapse = "")
  }
  
  if (is_replacement_fun(x$method[1])) {
    name <- str_replace(x$method, fixed("<-"), "")
    str_c(method(name), "(", arglist, ") <- value")
  } else {
    str_c(method(x$method), "(", arglist, ")")
  }
}