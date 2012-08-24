rocout <- function(tag, name, subclass) {
  structure(list(tag = tag, name = name), class = c(subclass, "rocout"))
}

write_output <- function(rocouts, rocblocks) {
  
}


write_output <- function(writer, rocblocks) {
  UseMethod("write_output")
}

write_out <- function(roccers, rocblocks) {
  out <- list()
  
  for (roccer in roccers) {
    rocout <- roccer$rocout
    type <- output_type(rocout)
    if (is.null(out[[type]])) out[[type]] <- list()
    
    for (rocblock in rocblocks) {
      tag <- rocblock$roc[[rocout$name]]
      if (is.null(tag)) return()
      
      path <- output_path(rocout, rocblock)
      n <- length(output[[type]][[path]]) + 1
      if (n == 1) output[[type]][[path]] <- list()
      output[[type]][[path]][[n]] <- rocout$tag(tag)
    }
  }
  
  writers <- names(out)
  for (writer in writers) {
    match.fun(writer)(out[[writer]])
  }
  
}

output_path <- function(writer, rocblock) {
  UseMethod("output_path")
}

output_type <- function(writer, rocblock) {
  UseMethod("output_type")
}



# @return a list of the same length as \code{rocblocks}
process_output <- function(rocout, rocblocks) {
  # Name should have been set by roccer
  stopifnot(!is.null(rocout$name))
  
  lapply(rocblocks, function(rocblock) {
    tag <- rocblock$roc[[rocout$name]]
    if (is.null(tag)) return()
    rocout$tag(tag)
  })
}

