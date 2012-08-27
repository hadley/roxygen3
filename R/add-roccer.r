#' @export
add_roccer <- function(name, parser = NULL, output = NULL, env = parent.frame()) { 
  roc <- roccer(name, parser, output)
  assign(str_c("@", name), roc, envir = env)
}

#' @export
add_tag_roccer <- function(name, input, command = name, env = parent.frame()) {
  add_roccer(name, 
    roc_parser(tag = input), 
    rd_out(rd_command(command)), 
    env = env)
}

#' @export
add_ns_roccer <- function(name, input, ns_out, env = parent.frame()) {
  add_roccer(name, 
    roc_parser(tag = input),
    namespace_out(ns_out), 
    env)
}
