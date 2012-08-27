# Translate a command and expressions into an Rd expression;
# multiple expressions take their own braces.
#
# commands have two methods: \code{merge} and \code{format}.  Currently for all
# commands, merge just combines all values, and format selects from these to 
# display the commands in the appropriate way. 
#
#' @auto_imports
new_command <- function(command, values) {
  if (is.null(values)) return()
  
  subc <- str_c(command, "_command")
  list(structure(list(command = command, values = values), class = c(subc, "rd_command")))
}

is.rd_command <- function(x) inherits(x, "rd_command")

#' @auto_imports
escape_comments <- function(x) str_replace_all(x, fixed("%"), "\\%")

#' @export
print.rd_command <- function(x, ...) {
  cat(format(x), "\n")
}

# Translate a command and values into an Rd expression; multiple values get their
# own braces.
make_rd_command <- function(command, ..., space = FALSE) {
  if (space) {
    values <- str_c("\n", str_c(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }
  # Turn non-breaking spaces back into regular spaces
  values <- str_replace_all(values, fixed("\u{A0}"), " ")
  str_c("\\", command, str_c("{", values, "}", collapse = ""), "\n")                         
}

#' @export
format.rd_command <- function(x, ...) {
  stop("Unimplemented format: ", class(x)[1], call. = FALSE)
}

#' @export
merge.rd_command <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))  
  new_command(x$command, c(x$values, y$values))
}

# commands that repeat multiple times --------------------------------------------

format_rd <- function(x, ...) {
  out <- vapply(sort(unique(x$values)), make_rd_command, command = x$command, 
    FUN.VALUE = character(1), USE.NAMES = FALSE)
  str_c(out, collapse = "")
}

#' @export
format.keyword_command <- format_rd

#' @export
format.alias_command <- function(x, ...) {
  x$values <- escape_comments(x$values)
  format_rd(x)
}

#' @export
format.comment_command <- function(x, ...) {
  str_c("% " , x$values, collapse = "\n\n")
}

# commands that keep the first occurence -----------------------------------------
format_first <- function(x, ...) {
  make_rd_command(x$command, x$values[1])
} 

#' @export
format.name_command <- function(x, ...) {
  x$values <- escape_comments(x$values)
  format_first(x, ...)
}
#' @export
format.title_command <- format_first

#' @export
format.format_command <- format_first

#' @export
format.encoding_command <- format_first

# commands collapse their values into a single string ----------------------------

format_collapse <- function(x, ..., indent = 2, exdent = 2) {
  values <- str_c(x$values, collapse = "\n\n")
  make_rd_command(x$command, str_wrap(values, width = 60, indent = indent, 
    exdent = exdent), space = TRUE)
} 
#' @export
format.author_command <- format_collapse

#' @export
format.concept_command <- format_collapse

#' @export
format.description_command <- format_collapse

#' @export
format.details_command <- format_collapse

#' @export
format.note_command <- format_collapse

#' @export
format.references_command <- format_collapse

#' @export
format.seealso_command <- format_collapse

#' @export
format.source_command <- format_collapse

#' @export
format.usage_command <- function(x, ...) {
  x$values <- vapply(x$values, format, character(1))
  format_collapse(x, ..., exdent = 4)
}

#' @export
format.value_command <- format_collapse


# commands that don't have output ------------------------------------------------

format_null <- function(x, ...) NULL

#' @export
format.family_command <- format_null

#' @export
format.inheritParams_command <- format_null

#' @export
format.formals_command <- format_null

# commands with special errors or other semantics --------------------------------

#' @export
format.arguments_command <- function(x, ...) {
  names <- names(x$values)
  dups <- duplicated(names)
  
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  make_rd_command("arguments", str_wrap(items, width = 60, exdent = 2, indent = 2),
    space = TRUE)
}

#' @export
#' @auto_imports
format.slot_command <- function(x, ...) {
  names <- names(x$values)
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  str_c("\\section{Slots}\n\n",
    "\\itemize{\n", 
    str_wrap(items, width = 60, exdent = 2, indent = 2),
    "\n}\n")
}

#' @export
format.section_command <- function(x, ...) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)
  
  setions <- str_c("\\section{", names, "}{\n", contents, "\n}\n", 
    collapse = "\n")
}

#' @export
format.examples_command <- function(x, ...) {
  values <- str_c(x$values, collapse = "\n")
  escaped <- escape_comments(x$values)
  make_rd_command(x$command, escaped, space = TRUE)  
}
