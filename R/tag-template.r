#' Include documentation templates.
#'
#' Templates should be stored in \file{man-roxygen/} with a \file{.r} extension.
#' Templates are parsed with \code{\link[brew]{brew}}, so you can access
#' template variables using \code{<%= name %>}.
#'
#' @usageTag
#'   @@template name_of_template
#'   @@templateVar name value
#' @rdname TemplateTag
setClass("TemplateTag", contains = "Tag", representation(
  contents = "character"))
setMethod("getPrereqs", "TemplateTag", function(x) "TemplateVarTag")

setMethod("process", "TemplateTag", function(input, block) {
  templates <- tag_value(block, "template")
  paths <- vapply(templates, template_find, FUN.VALUE = character(1))

  raw_vars <- tag_value(block, "templateVar")
  vars <- lapply(raw_vars, type.convert, as.is = TRUE)

  results <- unlist(lapply(paths, template_eval, vars = list2env(vars)))
  tags <- parse_roc(as.character(ref), base_tags())

  input@contents <- results
  tag(block, "template") <- NULL
  tag(block, "templateVar") <- NULL
  block
})

# Will need to test that it affects all tags (e.g. @name, @rdname)
# Make sure it combines with existing tags
# Need to make sure the outputs are also processed - and check why I cared about
# the ordering of the inputs.

#' @rdname TemplateTag
setClass("TemplateVarTag", contains = "Tag",
  representation(variables = character()))
setMethod("value", "TemplateVarTag", function(tag) tag@variables)
setMethod("value<-", "TemplateVarTag", function(tag, value) {
  pieces <- str_split_fixed(value, "[[:space:]]+", 2)

  tag@variables <- setNames(pieces[, 1], str_trim(pieces[, 2]))
  tag
})

template_find <- function(template_name) {
  path <- file.path("man-roxygen", str_c(template_name, ".r"))
  if (!file.exists(path)) {
    stop("Can not find template ", template_name, call. = FALSE)
  }
  path
}

