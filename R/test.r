test_process <- function(text, behaviour = no_output()) {  
  bundle <- parse_block(text)
  blocks <- process(bundle)@blocks
  blocks[[length(blocks)]]@tags
}

test_rd <- function(text) {
  bundle <- parse_block(text)
  bundle <- process(bundle)
  build_rd(bundle@blocks)
}
test_ns <- function(text) {
  bundle <- parse_block(text)
  bundle <- process(bundle)
  build_namespace(bundle@blocks)
}

#' Parse and execute a block of text in a package like environment.
#'
#' This is used cheifly for testing.
#'
#' @param text code to parse/execute
#' @autoImports
#' @export
parse_block <- function(text, behaviour = no_output()) {
  pkg_dummy <- structure(
    list(path = tempfile(), package = "temp", version = 0.01), 
    class = "package")  
  env <- devtools:::create_ns_env(pkg_dummy)
  on.exit(unload(pkg_dummy))

  src <- srcfilecopy(digest(text), text)
  expr <- parse(text = text, srcfile = src)
  eval(expr, env = env)

  lines <- str_split(text, "\n")[[1]]
  blocks <- parse_text(lines, env, src, tags = behaviour@tags)
  
  new("Bundle",
    blocks = blocks,
    behaviour = behaviour)  
}

no_output <- function() {
  new("Behaviour", 
    tags = base_tags(),
    processors = local_apropos("^process[A-Z_]"),
    writers = character()
  )
}
