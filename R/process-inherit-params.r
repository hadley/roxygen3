process_inherit_params <- function(package) {

  blocks <- package@blocks
  for(i in seq_along(blocks)) {
    obj <- blocks[[i]]@object@value
    tags <- blocks[[i]]@tags

    if (!is.function(obj)) next

    inherit_from <- tags$inheritParams
    if (is.null(inherit_from)) next

    inherited <- unlist(lapply(inherit_from@text, find_params, blocks))
    if (is.null(inherited)) {
      message("@inheritParams: can't find topic ", inherit_from)
      next
    }

    fun_params <- names(formals(obj))
    cur_params <- tags$param
    if (is.null(cur_params)) {
      cur_params <- new("ParamTag", arguments = character())
    }

    missing <- setdiff(fun_params, names(cur_params@arguments))
    matching <- intersect(missing, names(inherited))
    cur_params@arguments <- c(cur_params@arguments, inherited[matching])

    blocks[[i]]@tags$param <- cur_params
  }

  package@blocks <- blocks
  package
}

find_params <- function(name, blocks) {
  if (str_detect(name, fixed("::"))) {
    # Reference to another package
    pieces <- str_split(name, fixed("::"))[[1]]
    rd <- get_rd(pieces[2], pieces[1])
    if (is.null(rd)) return(NULL)

    rd_arguments(rd)
  } else {
    # Reference within this package
    matching_alias <- function(x) {
      aliases <- x@tags$aliases
      if (is.null(aliases)) return(FALSE)
      name %in% aliases@text
    }
    matches <- Filter(matching_alias, blocks)

    if (length(matches) != 1) return(NULL)
    matches[[1]]@tags$param@arguments
  }
}


get_rd <- function(topic, package = NULL) {
  help_call <- substitute(help(t, p), list(t = topic, p = package))
  top <- eval(help_call)
  if (length(top) == 0) return()

  utils:::.getHelpFile(top)
}

# rd_arguments(get_rd("mean"))
rd_arguments <- function(rd) {
  arguments <- get_tags(rd, "\\arguments")[[1]]
  items <- get_tags(arguments, "\\item")

  values <- lapply(items, function(x) rd2rd(x[[2]]))
  params <- vapply(items, function(x) rd2rd(x[[1]]), character(1))

  setNames(values, params)
}

get_tags <- function(rd, tag) {
  rd_tag <- function(x) attr(x, "Rd_tag")

  Filter(function(x) rd_tag(x) == tag, rd)
}

rd2rd <- function(x) {
  paste(unlist(tools:::as.character.Rd(x)), collapse = "")
}
