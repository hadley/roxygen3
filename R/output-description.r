description_out <- function(tag, name = NULL)  {
  rocout(tag, name, subclass = "description_out")
}

output_path.description_out <- function(writer, rocblock) {
  "DESCRIPTION" 
}

field <- function(name) {
  function(x) {
    list(setNames(list(x), name))
  }
}

#' @auto_imports
output_postproc.description_out <- function(output) {
  out <- unlist(output, recursive = FALSE)
  out$Collate <- str_c(out$Collate, collapse = "\n")
  
  out
}

output_write.description_out <- function(output, path) {
  old <- read_description(path)
  new <- modify_list(old, output)
  
  desc <- render_description(new)
  write_if_different(path, desc)
}

# Functions to manipulate the description file -------------------------------

read_description <- function(file) {
  dcf <- read.dcf(file)
  
  dcf_list <- setNames(as.list(dcf[1, ]), colnames(dcf))
  lapply(dcf_list, str_trim)
}

render_description <- function(desc, file) {
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
  
  fields <- Map(cat.description, names(desc), desc)
  str_c(unlist(fields), collapse = "\n")
}

# Print the field-value pair to a given file or standard out.

# Determine whether a given field is too long and should be text-wrapped
#' @auto_import
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