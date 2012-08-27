#' Add additional topic aliases.
#' 
#' Add additional aliases through which the user can find the documentation
#' with \code{\link{help}}. The topic name is always included in the list of
#' aliases.
#'
#' @usage @@aliases space separated aliases
add_tag_roccer("aliases", words_tag(min = 1), "alias")

#' Override the default topic name.
#' 
#' By default, the topic name is derived from the object following the 
#' roxygen comments. This tag is rarely needed - if you are using it often
#' it's probably an indication that you need to extend roxygen3 for you 
#' object type.
#'
#' @usage @@name name
add_roccer("name", 
  roc_parser(
    tag = words_tag(min = 1, max = 1),
    one = function(roc, obj, ...) {
      name <- roc$name %||% obj$name
      if (is.null(name)) {
        message("Missing name. This object will not be documented")
        return()
      }

      list(name = name, aliases = union(roc$aliases, name))
    }
  ),
  rd_out(rd_command("name"))
)
base_prereqs[["name"]] <- "aliases"