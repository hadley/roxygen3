#' Parse directory of source files.
#'
#' @param path path to directory of R files to parse
#' @param env environment that contains the results of evaluating the file.
#'   If \code{NULL}, will create a new environment with the global environment
#'   as a parent and will evaluate the code in that environment. This will be
#'   fine for simple, stand alone files, but you'll need to manage the 
#'   environments yourself for more complicated packages.
#' @dev
#' @export
parse_directory <- function(path, env = NULL) {
  r_files <- dir(path, pattern = "\\.[RrSs]$", full.names = TRUE)
  
  if (is.null(env)) {
    env <- new.env(parent = globalenv())
    lapply(r_files, sys.source, envir = env, chdir = TRUE)
  }
  
  unlist(lapply(r_files, parse_file, env = env), recursive = FALSE)
}

#' Parse a source file containing roxygen blocks.
#'
#' @param path path of file to parse
#' @inheritParams parse_directory
#' @return A list of roc objects
#' @dev
#' @export
parse_file <- function(path, env = NULL) {
  if (is.null(env)) {
    env <- new.env(parent = globalenv())
    sys.source(path, env, chdir = TRUE)
  }
  
  # Find the locations of (comment + code) blocks
  lines <- readLines(path, warn = FALSE)
  src <- srcfile(path)
  
  parse_text(lines, env, src)
}

#' Parse and execute a block of text in a package like environment.
#'
#' This is used cheifly for testing.
#'
#' @param text code to parse/execute
#' @auto_imports
#' @export
parse_block <- function(text) {
  pkg_dummy <- structure(
    list(path = tempfile(), package = "temp", version = 0.01), 
    class = "package")  
  env <- devtools:::create_ns_env(pkg_dummy)
  on.exit(unload(pkg_dummy))

  src <- srcfilecopy(digest(text), text)
  expr <- parse(text = text, srcfile = src)
  eval(expr, env = env)

  lines <- str_split(text, "\n")[[1]]
  
  parse_text(lines, env, src)
}

parse_text <- memoise(function(lines, env, src) {
  parsed <- parse(text = lines, src = src)
  refs <- attr(parsed, "srcref")
  
  # Walk through each src ref and match code and comments
  extract <- function(i) {
    # Comments begin after last line of last block, and continue to 
    # first line of this block
    beg <- if (i == 1) 1 else refs[[i - 1]][[3]] + 1 
    end <- refs[[i]][[1]] - 1

    roc <- parse_roc(lines[beg:end])
    obj <- object_from_call(parsed[[i]], env, refs[[i]])
    
    if (is.null(roc) && is.null(obj)) return()
    
    rocblock(obj = obj, roc = roc, path = src$filename, 
      lines = c(beg, end))
  }  
  compact(lapply(seq_along(parsed), extract))
})

#' @auto_imports
parse_roc <- function(lines, match = "^\\s*#+\' ?") {
  lines <- lines[str_detect(lines, match)]
  if (length(lines) == 0) return(NULL)
  
  trimmed <- str_replace(lines, match, "")
  joined <- str_c(trimmed, collapse = '\n')

  # If the comment block does not start with a @, then it must be the
  # introduction section
  if (!str_detect(joined, "^@")) {
    joined <- str_c("@intro ", joined)
  }

  ## Thanks to Fegis at #regex on Freenode for the
  ## lookahead/lookbehind hack; as he notes, however, "it's not
  ## proper escaping though... it will not split a@@@b."
  elements <- str_split(joined, perl('(?<!@)@(?!@)'))[[1]][-1]
  elements <- str_replace_all(elements, fixed("@@"), "@")
  
  cols <- str_split_fixed(elements, "[[:space:]]+", 2)
  cols[, 2] <- str_trim(cols[, 2])

  tapply(cols[, 2], cols[, 1], list)
  # TODO: init_tag here
}

find_tag <- function(name, text, block) {
  # find matching class for name
  class_name <- str_c("Tag", name)
  if (!isClass(class_name)) {
    message("Unknown tag @", name, " at ", location(block))
    return(NULL)
  }
  
  new(class_name, text = text)
}

