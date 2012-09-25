#' Include documentation templates.
#'
#' Templates should be stored in \file{man-roxygen/} with a \file{.r} extension.
#' Templates are parsed with \code{\link[brew]{brew}}, so you can access
#' template variables using \code{<%= name %>}.
#'
#' @tagUsage
#'   @@template name_of_template
#'   @@templateVar name value
#' @rdname TemplateTag
setClass("TemplateTag", contains = "Tag", representation(
  contents = "character"))
setMethod("getPrereqs", "TemplateTag", function(tag) "TemplateVarTag")

setMethod("process", "TemplateTag", function(input, block) {
  templates <- tag_value(block, "template")
  paths <- vapply(templates, template_find, FUN.VALUE = character(1))

  vars <- tag_value(block, "templateVar")
  text <- unlist(lapply(paths, template_eval, vars = vars), use.names = FALSE)

  tags <- parse_roc(text, base_tags(), block@srcref)

  block@tags <- c(block@tags, tags)
  tag(block, "template") <- NULL
  tag(block, "templateVar") <- NULL
  block
})

#' @rdname TemplateTag
setClass("TemplateVarTag", contains = "Tag",
  representation(variables = "list"))
setMethod("value", "TemplateVarTag", function(tag) tag@variables)
setMethod("value<-", "TemplateVarTag", function(tag, value) {
  pieces <- str_split_fixed(value, "[[:space:]]+", 2)

  vars <- lapply(pieces[, 2], type.convert, as.is = TRUE)

  tag@variables <- setNames(pieces[, 1], vars)
  tag
})

template_find <- function(template_name) {
  path <- file.path("man-roxygen", str_c(template_name, ".r"))
  if (!file.exists(path)) {
    message("Can not find template ", template_name)
    return()
  }
  path
}

#' @autoImports
template_eval <- function(template, vars) {
  if (is.null(vars)) {
    env <- new.env(parent = baseenv())
  } else {
    env <- list2env(vars, parent = baseenv())
  }
  capture.output(brew(template, envir = env))
}

