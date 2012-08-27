roc_alias <- basic_roccer("aliases", words_tag(min = 1), "alias")

roc_name <- roccer("name", 
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