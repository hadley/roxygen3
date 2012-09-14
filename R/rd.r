# Translate a command and expressions into an Rd expression;
# multiple expressions take their own braces.
#
# commands have two methods: \code{merge} and \code{format}.  Currently for all
# commands, merge just combines all values, and format selects from these to
# display the commands in the appropriate way.
#
#' @autoImports
#' @export
#' @dev
RdCommand <- function(command, values) {
  if (is.null(values)) return()

  class <- str_c(first_upper(command), "Command")
  new(class, name = command, values = values)
}

setClass("RdCommand", representation(
  name = "character",
  values = "character"
))

setMethod("show", "RdCommand", function(object) {
  cat(format(object), "\n")
})

merge_rd <- function(x, y) {
  stopifnot(identical(class(x), class(y)))
  RdCommand(x@name, c(x@values, y@values))
}

# Commands that are repeated for each value
setClass("RepeatedCommand", contains = "RdCommand")
setClass("AliasCommand", contains = "RepeatedCommand")
setClass("KeywordCommand", contains = "RepeatedCommand")

setMethod("format", "RepeatedCommand", function(x, ...) {
  values <- escape_comments(sort_c(unique(x@values)))
  str_c("\\", x@name, "{", values, "}")
})

# Commands that only display the first occurence
setClass("SingleCommand", contains = "RdCommand")
setClass("NameCommand", contains = "SingleCommand")
setClass("TitleCommand", contains = "SingleCommand")
setClass("FormatCommand", contains = "SingleCommand")
setClass("EncodingCommand", contains = "SingleCommand")

setMethod("format", "SingleCommand", function(x, ...) {
  str_c("\\", x@name, "{", escape_comments(x@values[1]), "}")
})

# Commands that collapse their values into a single string
setClass("CollapsingCommand", contains = "RdCommand")
setClass("FormatCommand", contains = "CollapsingCommand")
setClass("ConceptCommand", contains = "CollapsingCommand")
setClass("AuthorCommand", contains = "CollapsingCommand")
setClass("DescriptionCommand", contains = "CollapsingCommand")
setClass("DetailsCommand", contains = "CollapsingCommand")
setClass("NoteCommand", contains = "CollapsingCommand")
setClass("ReferencesCommand", contains = "CollapsingCommand")
setClass("SeealsoCommand", contains = "CollapsingCommand")
setClass("SourceCommand", contains = "CollapsingCommand")
setClass("ValueCommand", contains = "CollapsingCommand")

setMethod("format", "CollapsingCommand", function(x, ...) {
  wrapped <- rd_wrap(x@values)
  escaped <- escape_comments(wrapped)

  str_c("\\", x@name, "{\n", wrapped, "\n}")
})

# Commands with special semantics ----------------------------------------------

setClass("UsageCommand", contains = "RdCommand")
setMethod("format", "UsageCommand", function(x, ...) {
  wrapped <- code_wrap(unlist(lapply(x@values, format)))
  escaped <- escape_comments(wrapped)

  str_c("\\", x@name, "{\n", escaped, "\n}")
})

setClass("CommentCommand", contains = "RdCommand")
setMethod("format", "CommentCommand", function(x) {
  str_c("% " , x@values, collapse = "\n\n")
})

setClass("ArgumentsCommand", contains = "RdCommand")
setMethod("format", "ArgumentsCommand", function(x, ...) {
  arg_names <- escape_comments(names(x@values))
  arg_values <- escape_comments(x@values)

  items <- str_c("\\item{", arg_names, "}{", arg_values, "}")
  str_c("\\arguments{\n", rd_wrap(items), "\n}\n")
})

setClass("SlotCommand", contains = "RdCommand")
setMethod("format", "SlotCommand", function(x, ...) {
  slot_names <- escape_comments(names(x@values))
  slot_values <- escape_comments(x@values)

  items <- str_c("\\item{", slot_names, "}{", slot_values, "}")
  str_c("\\section{Slots}\n\n", "\\itemize{\n", rd_wrap(items), "\n}")
})

setClass("SectionCommand", contains = "RdCommand")
setMethod("format", "SectionCommand", function(x, ...) {
  names <- escape_comments(names(x@values))
  values <- escape_comments(vapply(x@values, rd_wrap, character(1)))

  setions <- str_c("\\section{", names, "}{\n", values, "\n}\n",
    collapse = "\n")
})

setClass("ExamplesCommand", contains = "RdCommand")
setMethod("format", "ExamplesCommand", function(x, ...) {
  values <- str_c(escape_comments(x@values), collapse = "\n")
  str_c("\\examples{\n", values, "\n}\n")
})

setClass("DocTypeCommand", contains = "RdCommand")
setMethod("format", "DocTypeCommand", function(x, ...) {
  vals <- unique(x@values)
  if (length(vals) != 1) stop("Documentation can only have single docType")

  ok <- c("data", "package", "methods", "class")
  vals <- intersect(vals, ok)
  if (length(vals) == 0) return("")

  str_c("\\docType{", vals, "}")
})

setClass("PathCommand", contains = "RdCommand")
setMethod("format", "PathCommand", function(x, ...) {
  paths <- str_c(x@values, collapse = ", ")
  paste("% Generated from [", paths, "]", sep = "")
})


#' @autoImports
escape_comments <- function(x) str_replace_all(x, fixed("%"), "\\%")
