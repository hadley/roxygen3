usage.s3generic <- usage.function

usage.s3method <- function(obj, name) {
  args <- usage_args(formals(obj))
  
  new_usage(
    method = s3_method_info(obj),
    args = args, 
    subclass = "usage_s3method")
}

format.usage_s3method <- function(x) {
  str_c("\\method", str_c("{", x$method, "}", collapse = ""), 
    "(", args_string(x$args), ")")
}