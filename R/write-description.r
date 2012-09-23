
setMethod("writeDescription", "PackageBundle", function(object) {
  in_dir(object@path, callNextMethod())
})
setMethod("writeDescription", "Bundle", function(object) {
  desc <- build_description(object@blocks)
  write_description(desc)
})
setMethod("writeDescription", "Block", function(object) {
  compact(lapply(object@tags, writeDescription))
})
setMethod("writeDescription", "Tag", function(object) NULL)

build_description <- function(blocks) {
  output <- lapply(blocks, writeDescription)
  out <- unlist(output, recursive = FALSE)
  if (!is.null(out$Collate)) {
    out$Collate <- str_c(out$Collate, collapse = "\n")
  }

  out
}

write_description <- function(output) {
  old <- read_description("DESCRIPTION")
  new <- modify_list(old, output)

  desc <- render_description(new)
  write_if_different("DESCRIPTION", desc)
}

field <- function(name) {
  function(x) {
    list(setNames(list(x), name))
  }
}


# Functions to manipulate the description file -------------------------------

read_description <- function(file) {
  dcf <- read.dcf(file)

  dcf_list <- setNames(as.list(dcf[1, ]), colnames(dcf))
  lapply(dcf_list, str_trim)
}

render_description <- function(desc) {
  cat.description <- function(field, value) {
    comma_sep <- any(field %in% c("Suggests", "Depends", "Extends", "Imports"))
    individual_lines <- field %in% c("Collate")

    if (comma_sep) {
      value <- str_trim(str_split(value, ",\\s+")[[1]])
      value_string <- str_c("    ", value, collapse = ",\n")
      str_c(field, ":\n", value_string)
    } else {
      width <- if (individual_lines) 0 else 80
      wrap_field_if_necessary(field, value, wrap.threshold = width)
    }
  }
  desc <- Filter(function(x) length(x) > 0 && !identical(x, ""), desc)
  fields <- Map(cat.description, names(desc), desc)
  str_c(unlist(fields), collapse = "\n")
}

# Print the field-value pair to a given file or standard out.

# Determine whether a given field is too long and should be text-wrapped
#' @autoImports
wrap_field_if_necessary <- function(field, value, wrap.threshold) {
   text <- simulate_formatted_text(field, value)
   longest.line <- max(str_length(text))

   if (longest.line > wrap.threshold) {
     text <- str_wrap(str_c(field, ": ", value), exdent = 4,
      width = wrap.threshold)
   }

   return(text)
}

# Simulate what was probably the user's intended field formatting
simulate_formatted_text <- function(field, value) {
  text     <- str_split(str_c(field, ": ", value), "\n")[[1]]
  text[-1] <- str_c("    ", text[-1]) # indents all *but* the first line

  return(text)
}
